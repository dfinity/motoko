(*
This module provides some convenience to assemble WASM instruction lists. The
features are

 * O(1) concatenation (using difference list internally)
 * Managing of label depths.
 * Some simple peephole optimizations.
*)

open Wasm.Types
open Wasm.Ast
open Wasm.Source

(* Some simpl peephole optimizations, to make the output code look less stupid *)
(* This uses a zipper.*)
let optimize : instr list -> instr list = fun is ->
  let rec go l r = match l, r with
    (* Loading and dropping is pointless *)
    | { it = Const _; _} :: l', { it = Drop; _ } :: r' -> go l' r'
    | { it = GetLocal _; _} :: l', { it = Drop; _ } :: r' -> go l' r'
    (* The following is not semantics preserving for general Wasm (due to out-of-memory)
       but should be fine for the code that we create *)
    | { it = Load _; _} :: l', { it = Drop; _ } :: _ -> go l' r
    (* This can erase the arguments in a cascading manner. *)
    | { it = Binary _; _} :: l', ({ it = Drop; _ } as i) :: r' ->
      go l' (i :: i :: r')
    (* Introduce TeeLocal *)
    | { it = SetLocal n1; _} :: l', ({ it = GetLocal n2; _ } as i) :: r' when n1 = n2 ->
      go l' ({i with it = TeeLocal n2 } :: r')
    (* Eliminate TeeLocal followed by Drop (good for confluence) *)
    | ({ it = TeeLocal n; _} as i) :: l', { it = Drop; _ } :: r' ->
      go l' ({i with it = SetLocal n } :: r')
    (* Code after Return is dead *)
    | _, ({ it = Return; _ } as i) :: _ -> List.rev (i::l)
    (* Look further *)
    | _, i::r' -> go (i::l) r'
    (* Done looking *)
    | l, [] -> List.rev l
  in go [] is

(* When we do not care about the generate source region *)
let nr x = x @@ Wasm.Source.no_region

(* The main type of this module *)
type t = int32 -> instr list -> instr list

let to_instr_list (is : t) : instr list =
  optimize (is 0l [])

let to_nested_list d is =
  optimize (is Int32.(add d 1l) [])

(* The concatenation operator *)
let nop : t = fun _ rest -> rest
let (^^) (is1 : t) (is2 : t) : t = fun d rest -> is1 d (is2 d rest)

(* Singletons *)
let i (instr : instr) : t = fun _ rest -> instr :: rest
let i_ (instr : instr') = i (instr @@ Wasm.Source.no_region)

(* map and concat *)
let concat_map f xs = List.fold_right (^^) (List.map f xs) nop
let concat_mapi f xs = List.fold_right (^^) (List.mapi f xs) nop
let table n f = List.fold_right (^^) (Lib.List.table n f) nop

(* Depths-managing combinators *)

let if_ (ty : stack_type) (thn : t) (els : t) : t =
  fun d rest ->
    nr (If (ty, to_nested_list d thn, to_nested_list d els)) :: rest

let block_ (ty : stack_type) (body : t) : t =
  fun d rest ->
    nr (Block (ty, to_nested_list d body)) :: rest

let loop_ (ty : stack_type) (body : t) : t =
  fun d rest ->
    nr (Loop (ty, to_nested_list d body)) :: rest

(* Remember depth *)
type depth = int32 Lib.Promise.t

let new_depth_label () : depth =  Lib.Promise.make_named "depth_label"

let remember_depth depth (is : t) : t =
  fun d rest -> Lib.Promise.fulfill depth d; is d rest

let with_current_depth (k : depth -> t) : t =
  let depth = new_depth_label () in
  remember_depth depth (k depth)

let branch_to_ (p : depth) : t =
  fun d rest ->
    nr (Br (nr Int32.(sub d (Lib.Promise.value p)))) :: rest

(* Convenience combinations *)

let labeled_block_ (ty : stack_type) depth (body : t) : t =
  block_ ty (remember_depth depth body)
