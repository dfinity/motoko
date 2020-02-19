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
open Wasm.Types

let combine_shifts const op = function
  | I32 opl, ({it = I32 l'; _} as cl), I32 opr, I32 r' when opl = opr ->
    let l, r = Int32.(to_int l', to_int r') in
    if (l >= 0 && l < 32 && r >= 0 && r < 32 && l + r < 32) then
      Some [{const with it = Const {cl with it = I32 (Int32.add l' r')}}; {op with it = Binary (I32 opl)}]
    else None
  | _ -> None

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
    | _, ({ it = Return | Br _ | Unreachable; _ } as i) :: t ->
      let fishDwarf = List.find_opt (fun i -> Wasm_exts.CustomModuleEncode.dwarf_like i.at) in
      List.rev (match fishDwarf t with
                | Some dw -> dw::i::l
                | None -> i::l)
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
        when Option.is_some (combine_shifts const op (opl, cl, opr, cr.it)) ->
      go l' (Option.get (combine_shifts const op (opl, cl, opr, cr.it)) @ r')
    (* Null shifts can be eliminated *)
    | l', {it = Const {it = I32 0l; _}; _} :: {it = Binary (I32 I32Op.(Shl|ShrS|ShrU)); _} :: r' ->
      go l' r'

    | l', ({it = Nop; _} as n1) :: (({it = Nop; _} as n2) :: _ as r') when
          Wasm_exts.CustomModuleEncode.is_dwarf_statement n1.at
          && Wasm_exts.CustomModuleEncode.is_dwarf_statement n2.at ->
      go l' r'
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
let concat xs = List.fold_right (^^) xs nop
let concat_map f xs = List.fold_right (^^) (List.map f xs) nop
let concat_mapi f xs = List.fold_right (^^) (List.mapi f xs) nop
let table n f = List.fold_right (^^) (Lib.List.table n f) nop

(* Region-managing combinator *)

let cr at =
  let left = Wasm.Source.{
    file = at.Source.left.Source.file;
    line = at.Source.left.Source.line;
    column = at.Source.left.Source.column } in
  let right = Wasm.Source.{
    file = at.Source.right.Source.file;
    line = at.Source.right.Source.line;
    column = at.Source.right.Source.column } in
  Wasm.Source.{ left; right }

let with_region (pos : Source.region) (body : t) : t =
  fun d _pos rest -> body d (cr pos) rest

(* Depths-managing combinators *)

let if_ (ty : stack_type) (thn : t) (els : t) : t =
  fun d pos rest ->
    (If (ty, to_nested_list d pos thn, to_nested_list d pos els) @@ pos) :: rest

let block_ (ty : stack_type) (body : t) : t =
  fun d pos rest ->
    (Block (ty, to_nested_list d pos body) @@ pos) :: rest

let loop_ (ty : stack_type) (body : t) : t =
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

let labeled_block_ (ty : stack_type) depth (body : t) : t =
  block_ ty (remember_depth depth body)

(* Intended to be used within assert *)

let is_nop (is : t) =
  is 0l Wasm.Source.no_region [] = []

(* DWARF attributes *)

open Wasm_exts.Dwarf5
open Wasm_exts.Abbreviation

type dw_AT = Producer of string
           | Language of int
           | Name of string
           | Stmt_list of int
           | Comp_dir of string
           | Use_UTF8 of bool
           | Low_pc of int
           | High_pc of int
           | Addr_base of int
           | Ranges
           | Decl_line of int
           | Decl_column of int
           | Prototyped of bool
           | External of bool
           | Byte_size of int
           | Bit_size of int
           | Data_bit_offset of int
           | Discr of int (* reference *)
           | Discr_list
           | Discr_value
           | Artificial of bool

(* DWARF tags *)

type dw_TAG = Compile_unit of string * string (* compilation directory, file name *)
            | Subprogram of string * Source.pos
            | Formal_parameter of string * Source.pos
            | Variable
            | Typedef
            | Structure_type
            | Member
            | Variant_part
            | Variant

(* DWARF high-level structures *)

(* fakeFile gives a fake instruction that encodes a string for a
   DWARF attribute *)
let fakeFile (file : string) attr instr' : t =
  let fakeLoc = Wasm.Source.{ file; line = -attr; column = 0 } in
  fun _ _ instrs ->
  (instr' @@ Wasm.Source.{ left = fakeLoc; right = no_pos }) :: instrs

(* fakeColumn gives a fake instruction that encodes a single integer for a
   DWARF attribute *)
let fakeColumn (column : int) attr instr' : t =
  let fakeLoc = Wasm.Source.{ file = ""; line = -attr; column } in
  fun _ _ instrs ->
  (instr' @@ Wasm.Source.{ left = fakeLoc; right = no_pos }) :: instrs

let dw_attr : dw_AT -> t =
  function
  | Producer p -> fakeFile p dw_AT_producer Nop
  | Language l -> fakeColumn l dw_AT_language Nop
  | Name n -> fakeFile n dw_AT_name Nop
  | Stmt_list l -> fakeColumn l dw_AT_stmt_list Nop
  | Comp_dir n -> fakeFile n dw_AT_comp_dir Nop
  | Use_UTF8 b -> fakeColumn (if b then 1 else 0) dw_AT_use_UTF8 Nop
  | Low_pc l -> fakeColumn l dw_AT_low_pc Nop
  | High_pc h -> fakeColumn h dw_AT_high_pc Nop
  | Addr_base b -> fakeColumn b dw_AT_addr_base Nop
  | Ranges -> fakeColumn 0 dw_AT_ranges Nop
  | Decl_line l -> fakeColumn l dw_AT_decl_line Nop
  | Decl_column c -> fakeColumn c dw_AT_decl_column Nop
  | Prototyped b -> fakeColumn (if b then 1 else 0) dw_AT_prototyped Nop
  | External b -> fakeColumn (if b then 1 else 0) dw_AT_external Nop
  | Byte_size s -> fakeColumn s dw_AT_byte_size Nop
  | Bit_size s -> fakeColumn s dw_AT_bit_size Nop
  | Data_bit_offset o -> fakeColumn o dw_AT_data_bit_offset Nop
  | Artificial b -> fakeColumn (if b then 1 else 0) dw_AT_artificial Nop

(* emit a DW_TAG
   When it admits children, these follow sequentially,
   closed by dw_tag_children_done.
 *)

let dw_tag_children_done : t =
  let left = { Wasm.Source.no_pos with line = -1 } in
  let right = Wasm.Source.no_pos in
  fun _ _ x -> (Nop @@ { left; right }) :: x


let dw_tag : dw_TAG -> t =
  let fakeBlock tag attrs =
    fakeColumn 0 tag (Block ([], attrs 0l Wasm.Source.no_region [])) in
  function
  | Compile_unit (dir, file) ->
    let base_types =
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Bool") ^^
         dw_attr (Bit_size 1) ^^
         dw_attr (Data_bit_offset 0)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Char") ^^
         dw_attr (Bit_size 21) ^^
         dw_attr (Data_bit_offset 8)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Word8") ^^
         dw_attr (Bit_size 8) ^^
         dw_attr (Data_bit_offset 24)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Nat8") ^^
         dw_attr (Bit_size 8) ^^
         dw_attr (Data_bit_offset 24)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Int8") ^^
         dw_attr (Bit_size 8) ^^
         dw_attr (Data_bit_offset 24)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Word16") ^^
         dw_attr (Bit_size 16) ^^
         dw_attr (Data_bit_offset 16)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Nat16") ^^
         dw_attr (Bit_size 16) ^^
         dw_attr (Data_bit_offset 16)) ^^
      fakeBlock dw_TAG_base_type
        (dw_attr (Name "Int16") ^^
         dw_attr (Bit_size 16) ^^
         dw_attr (Data_bit_offset 16))
    in
    let builtin_types =
      fakeBlock dw_TAG_structure_type
        (dw_attr (Name "Nat") ^^
         dw_attr (Byte_size 4)) ^^
      fakeBlock dw_TAG_member_Pointer_mark
        (dw_attr (Name "@pointer_mark") ^^
         dw_attr (Artificial true) ^^
         dw_attr (Bit_size 1) ^^
         dw_attr (Data_bit_offset 1)) ^^
      dw_tag_children_done
    in
    fakeBlock dw_TAG_compile_unit
      (dw_attr (Producer "DFINITY Motoko compiler, version 0.1") ^^
       dw_attr (Language dw_LANG_Swift) ^^ (* FIXME *)
       dw_attr (Name file) ^^
       dw_attr (Stmt_list 0) ^^
       dw_attr (Comp_dir dir) ^^
       dw_attr (Use_UTF8 true) ^^
       dw_attr (Low_pc 0) ^^
       dw_attr (Addr_base 8) ^^ (* FIXME: hardcoded *)
       dw_attr Ranges) ^^
      base_types ^^
      builtin_types
  | Subprogram (name, pos) ->
    fakeBlock dw_TAG_subprogram
      (dw_attr (Low_pc 0) ^^
       dw_attr (Name name) ^^
       dw_attr (Decl_line pos.Source.line) ^^
       dw_attr (Decl_column pos.Source.column) ^^
       dw_attr (Prototyped true) ^^
       dw_attr (External false))
  | Formal_parameter (name, pos) ->
    fakeBlock dw_TAG_formal_parameter
      (dw_attr (Name name) ^^
       dw_attr (Decl_line pos.Source.line) ^^
       dw_attr (Decl_column pos.Source.column))
  | _ -> assert false

let dw_tag_no_children = dw_tag

(* Marker for statement boundaries *)
let dw_statement { Source.left; Source.right } =
  let open Wasm.Source in
  let left = { file = left.Source.file; line = left.Source.line; column = left.Source.column } in
  (* right is only differing in the negated line *)
  let right = { left with line = - left.line } in
  (* FIXME *)assert (left.file = "" || Wasm_exts.CustomModuleEncode.is_dwarf_statement { left; right });
  fun _ _ x -> (Nop @@ { left; right }) :: x
