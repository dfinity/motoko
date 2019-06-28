(*
This module provides some convenience to assemble WASM instruction lists. The
features are

 * O(1) concatenation (using difference list internally)
 * Managing of label depths.
 * Some simple peephole optimizations.
*)

open Wasm.Ast
open Wasm.Source
open Wasm.Values

let shift_combinable cl cr =
  function
  | I32 I32Op.Shl, I32 I32Op.Shl
  | I32 I32Op.ShrS, I32 I32Op.ShrS
  | I32 I32Op.ShrU, I32 I32Op.ShrU ->
    begin match cl, cr with
    | I32 cl, I32 cr ->
      let l, r = Int32.(to_int cl, to_int cr) in
      l >= 0 && l < 32 && r >= 0 && r < 32 && l + r < 32
    | _ -> assert false
    end
  | _ -> false

let combine_shifts const op = function
  | I32 opl, ({it = I32 l; _} as cl), I32 opr, I32 r when opl = opr ->
    let amount = Int32.add l r in
    if Int32.(compare zero amount) = 0 then assert false
    else [{const with it = Const {cl with it = I32 amount}}; {op with it = Binary (I32 opl)}]
  | _ -> assert false


(* Some simple peephole optimizations, to make the output code look less stupid *)
(* This uses a zipper.*)
let optimize : instr list -> instr list = fun is ->
  let rec go l r = match l, r with
    (* Loading and dropping is pointless *)
    | { it = Const _ | LocalGet _; _} :: l', { it = Drop; _ } :: r' -> go l' r'
    (* The following is not semantics preserving for general Wasm (due to out-of-memory)
       but should be fine for the code that we create *)
    | { it = Load _; _} :: l', { it = Drop; _ } :: _ -> go l' r
    (* Introduce TeeLocal *)
    | { it = LocalSet n1; _} :: l', ({ it = LocalGet n2; _ } as i) :: r' when n1 = n2 ->
      go l' ({i with it = LocalTee n2 } :: r')
    (* Eliminate LocalTee followed by Drop (good for confluence) *)
    | ({ it = LocalTee n; _} as i) :: l', { it = Drop; _ } :: r' ->
      go l' ({i with it = LocalSet n } :: r')
    (* Code after Return, Br or Unreachable is dead *)
    | _, ({ it = Return | Br _ | Unreachable; _ } as i) :: _ ->
      List.rev (i::l)
    (* `If` blocks after pushed constants are simplifiable *)
    | { it = Const {it = I32 0l; _}; _} :: l', ({it = If (res,_,else_); _} as i) :: r' ->
      go l' ({i with it = Block (res, else_)} :: r')
    | { it = Const {it = I32 _; _}; _} :: l', ({it = If (res,then_,_); _} as i) :: r' ->
      go l' ({i with it = Block (res, then_)} :: r')
    (* Empty block is redundant *)
    | l', ({ it = Block (_, []); _ }) :: r' -> go l' r'
    (* Constant shifts can be combined *)
    | {it = Binary (I32 I32Op.(Shl|ShrS|ShrU) as opl); _} :: {it = Const cl; _} :: l',
      ({it = Const cr; _} as const) :: ({it = Binary opr; _} as op) :: r'
        when shift_combinable cl.it cr.it (opl, opr) ->
      go l' (combine_shifts const op (opl, cl, opr, cr.it) @ r')
    (* Look further *)
    | _, i::r' -> go (i::l) r'
    (* Done looking *)
    | l, [] -> List.rev l
  in go [] is

(* The main type of this module:
   Arguments for the current depth and the current source region,
   and producing a difference list *)
type t = int32 -> Wasm.Source.region -> instr list -> instr list

let to_instr_list (is : t) : instr list =
  optimize (is 0l Wasm.Source.no_region [])

let to_nested_list d pos is =
  optimize (is Int32.(add d 1l) pos [])


(* The concatenation operator *)
let nop : t = fun _ _ rest -> rest
let (^^) (is1 : t) (is2 : t) : t = fun d pos rest -> is1 d pos (is2 d pos rest)

(* Singletons *)
let i (instr : instr') : t = fun _ pos rest -> (instr @@ pos) :: rest

(* map and concat *)
let concat_map f xs = List.fold_right (^^) (List.map f xs) nop
let concat_mapi f xs = List.fold_right (^^) (List.mapi f xs) nop
let table n f = List.fold_right (^^) (Lib.List.table n f) nop

(* Region-managing combinator *)

let cr at =
  let left = { Wasm.Source.file = at.Source.left.Source.file;
    Wasm.Source.line = at.Source.left.Source.line;
    Wasm.Source.column = at.Source.left.Source.column } in
  let right = { Wasm.Source.file = at.Source.right.Source.file;
    Wasm.Source.line = at.Source.right.Source.line;
    Wasm.Source.column = at.Source.right.Source.column } in
  { Wasm.Source.left = left; Wasm.Source.right = right }

let with_region (pos : Source.region) (body : t) : t =
  fun d _pos rest -> body d (cr pos) rest

(* Depths-managing combinators *)

let if_ (ty : block_type) (thn : t) (els : t) : t =
  fun d pos rest ->
    (If (ty, to_nested_list d pos thn, to_nested_list d pos els) @@ pos) :: rest

let block_ (ty : block_type) (body : t) : t =
  fun d pos rest ->
    (Block (ty, to_nested_list d pos body) @@ pos) :: rest

let loop_ (ty : block_type) (body : t) : t =
  fun d pos rest ->
    (Loop (ty, to_nested_list d pos body) @@ pos) :: rest

(* Remember depth *)
type depth = int32 Lib.Promise.t

let new_depth_label () : depth =  Lib.Promise.make ()

let remember_depth depth (is : t) : t =
  fun d rest -> Lib.Promise.fulfill depth d; is d rest

let with_current_depth (k : depth -> t) : t =
  let depth = new_depth_label () in
  remember_depth depth (k depth)

let with_current_depth' (k : depth -> ('a * t)) : ('a * t) =
  let depth = new_depth_label () in
  let x, is = k depth in
  (x, remember_depth depth is)

let branch_to_ (p : depth) : t =
  fun d pos rest ->
    (Br (Int32.(sub d (Lib.Promise.value p)) @@ pos) @@ pos) :: rest

(* Convenience combinations *)

let labeled_block_ (ty : block_type) depth (body : t) : t =
  block_ ty (remember_depth depth body)

(* Intended to be used within assert *)

let is_nop (is :t) =
  is 0l Wasm.Source.no_region [] = []
