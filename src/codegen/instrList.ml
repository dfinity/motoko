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
        when Option.is_some (combine_shifts const op (opl, cl, opr, cr.it)) ->
      go l' (Option.get (combine_shifts const op (opl, cl, opr, cr.it)) @ r')
    (* Null shifts can be eliminated *)
    | l', {it = Const {it = I32 0l; _}; _} :: {it = Binary (I32 I32Op.(Shl|ShrS|ShrU)); _} :: r' ->
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


(* DWARF 5 constants *)

module Dwarf5 =
struct

let dw_TAG_array_type = 0x01
let dw_TAG_class_type = 0x02
let dw_TAG_entry_point = 0x03
let dw_TAG_enumeration_type = 0x04
let dw_TAG_formal_parameter = 0x05
(* let Reserved = 0x06 *)
(* let Reserved = 0x07 *)
let dw_TAG_imported_declaration = 0x08
(* let Reserved = 0x09 *)
let dw_TAG_label = 0x0a
let dw_TAG_lexical_block = 0x0b
(* let Reserved = 0x0c *)
let dw_TAG_member = 0x0d
(* let Reserved = 0x0e *)
let dw_TAG_pointer_type = 0x0f
let dw_TAG_reference_type = 0x10
let dw_TAG_compile_unit = 0x11
let dw_TAG_string_type = 0x12
let dw_TAG_structure_type = 0x13
(* let Reserved = 0x14 *)
let dw_TAG_subroutine_type = 0x15
let dw_TAG_typedef = 0x16
let dw_TAG_union_type = 0x17
let dw_TAG_unspecified_parameters = 0x18
let dw_TAG_variant = 0x19
let dw_TAG_common_block = 0x1a
let dw_TAG_common_inclusion = 0x1b
let dw_TAG_inheritance = 0x1c
let dw_TAG_inlined_subroutine = 0x1d
let dw_TAG_module = 0x1e
let dw_TAG_ptr_to_member_type = 0x1f
let dw_TAG_set_type = 0x20
let dw_TAG_subrange_type = 0x21
let dw_TAG_with_stmt = 0x22
let dw_TAG_access_declaration = 0x23
let dw_TAG_base_type = 0x24
let dw_TAG_catch_block = 0x25
let dw_TAG_const_type = 0x26
let dw_TAG_constant = 0x27
let dw_TAG_enumerator = 0x28
let dw_TAG_file_type = 0x29
let dw_TAG_friend = 0x2a
let dw_TAG_namelist = 0x2b
let dw_TAG_namelist_item = 0x2c
let dw_TAG_packed_type = 0x2d
let dw_TAG_subprogram = 0x2e
let dw_TAG_template_type_parameter = 0x2f
let dw_TAG_template_value_parameter = 0x30
let dw_TAG_thrown_type = 0x31
let dw_TAG_try_block = 0x32
let dw_TAG_variant_part = 0x33
let dw_TAG_variable = 0x34
let dw_TAG_volatile_type = 0x35
let dw_TAG_dwarf_procedure = 0x36
let dw_TAG_restrict_type = 0x37
let dw_TAG_interface_type = 0x38
let dw_TAG_namespace = 0x39
let dw_TAG_imported_module = 0x3a
let dw_TAG_unspecified_type = 0x3b
let dw_TAG_partial_unit = 0x3c
let dw_TAG_imported_unit = 0x3d
(* let Reserved = 0x3e *)
let dw_TAG_condition = 0x3f
let dw_TAG_shared_type = 0x40
let dw_TAG_type_unit = 0x41
let dw_TAG_rvalue_reference_type = 0x42
let dw_TAG_template_alias = 0x43
let dw_TAG_coarray_type = 0x44
let dw_TAG_generic_subrange = 0x45
let dw_TAG_dynamic_type = 0x46
let dw_TAG_atomic_type = 0x47
let dw_TAG_call_site = 0x48
let dw_TAG_call_site_parameter = 0x49
let dw_TAG_skeleton_unit = 0x4a
let dw_TAG_immutable_type = 0x4b
let dw_TAG_lo_user = 0x4080
let dw_TAG_hi_user = 0xffff


let dw_AT_sibling = 0x01
let dw_AT_location = 0x02
let dw_AT_name = 0x03
(* let Reserved = 0x04 *)
(* let Reserved = 0x05 *)
(* let Reserved = 0x06 *)
(* let Reserved = 0x07 *)
(* let Reserved = 0x08 *)
let dw_AT_ordering = 0x09
(* let Reserved = 0x0a *)
let dw_AT_byte_size = 0x0b
(* let Reserved = 0x0c *)
let dw_AT_bit_size = 0x0d
(* let Reserved = 0x0e *)
(* let Reserved = 0x0f *)
let dw_AT_stmt_list = 0x10
let dw_AT_low_pc = 0x11
let dw_AT_high_pc = 0x12
let dw_AT_language = 0x13
(* let Reserved = 0x14 *)
let dw_AT_discr = 0x15
let dw_AT_discr_value = 0x16
let dw_AT_visibility = 0x17
let dw_AT_import = 0x18
let dw_AT_string_length = 0x19
let dw_AT_common_reference = 0x1a
let dw_AT_comp_dir = 0x1b
let dw_AT_const_value = 0x1c
let dw_AT_containing_type = 0x1d
let dw_AT_default_value = 0x1e
(* let Reserved = 0x1f *)
let dw_AT_inline = 0x20
let dw_AT_is_optional = 0x21
let dw_AT_lower_bound = 0x22
(* let Reserved = 0x23 *)
(* let Reserved = 0x24 *)
let dw_AT_producer = 0x25
(* let Reserved = 0x26 *)
let dw_AT_prototyped = 0x27
(* let Reserved = 0x28 *)
(* let Reserved = 0x29 *)
let dw_AT_return_addr = 0x2a
(* let Reserved = 0x2b *)
let dw_AT_start_scope = 0x2c
(* let Reserved = 0x2d *)
let dw_AT_bit_stride = 0x2e
let dw_AT_upper_bound = 0x2f
(* let Reserved = 0x30 *)
let dw_AT_abstract_origin = 0x31
let dw_AT_accessibility = 0x32
let dw_AT_address_class = 0x33
let dw_AT_artificial = 0x34
let dw_AT_base_types = 0x35
let dw_AT_calling_convention = 0x36
let dw_AT_count = 0x37
let dw_AT_data_member_location = 0x38
let dw_AT_decl_column = 0x39
let dw_AT_decl_file = 0x3a
let dw_AT_decl_line = 0x3b
let dw_AT_declaration = 0x3c
let dw_AT_discr_list = 0x3d
let dw_AT_encoding = 0x3e
let dw_AT_external = 0x3f
let dw_AT_frame_base = 0x40
let dw_AT_friend = 0x41
let dw_AT_identifier_case = 0x42
(* let Reserved = 0x43 *)
let dw_AT_namelist_item = 0x44
let dw_AT_priority = 0x45
let dw_AT_segment = 0x46
let dw_AT_specification = 0x47
let dw_AT_static_link = 0x48
let dw_AT_type = 0x49
let dw_AT_use_location = 0x4a
let dw_AT_variable_parameter = 0x4b
let dw_AT_virtuality = 0x4c
let dw_AT_vtable_elem_location = 0x4d
let dw_AT_allocated = 0x4e
let dw_AT_associated = 0x4f
let dw_AT_data_location = 0x50
let dw_AT_byte_stride = 0x51
let dw_AT_entry_pc = 0x52
let dw_AT_use_UTF8 = 0x53
let dw_AT_extension = 0x54
let dw_AT_ranges = 0x55
let dw_AT_trampoline = 0x56
let dw_AT_call_column = 0x57
let dw_AT_call_file = 0x58
let dw_AT_call_line = 0x59
let dw_AT_description = 0x5a
let dw_AT_binary_scale = 0x5b
let dw_AT_decimal_scale = 0x5c
let dw_AT_small = 0x5d
let dw_AT_decimal_sign = 0x5e
let dw_AT_digit_count = 0x5f
let dw_AT_picture_string = 0x60
let dw_AT_mutable = 0x61
let dw_AT_threads_scaled = 0x62
let dw_AT_explicit = 0x63
let dw_AT_object_pointer = 0x64
let dw_AT_endianity = 0x65
let dw_AT_elemental = 0x66
let dw_AT_pure = 0x67
let dw_AT_recursive = 0x68
let dw_AT_signature = 0x69
let dw_AT_main_subprogram = 0x6a
let dw_AT_data_bit_offset = 0x6b
let dw_AT_const_expr = 0x6c
let dw_AT_enum_class = 0x6d
let dw_AT_linkage_name = 0x6e
let dw_AT_string_length_bit_size = 0x6f
let dw_AT_string_length_byte_size = 0x70
let dw_AT_rank = 0x71
let dw_AT_str_offsets_base = 0x72
let dw_AT_addr_base = 0x73
let dw_AT_rnglists_base = 0x74
(* let Reserved = 0x75 *)
let dw_AT_dwo_name = 0x76
let dw_AT_reference = 0x77
let dw_AT_rvalue_reference = 0x78
let dw_AT_macros = 0x79
let dw_AT_call_all_calls = 0x7a
let dw_AT_call_all_source_calls = 0x7b
let dw_AT_call_all_tail_calls = 0x7c
let dw_AT_call_return_pc = 0x7d
let dw_AT_call_value = 0x7e
let dw_AT_call_origin = 0x7f
let dw_AT_call_parameter = 0x80
let dw_AT_call_pc = 0x81
let dw_AT_call_tail_call = 0x82
let dw_AT_call_target = 0x83
let dw_AT_call_target_clobbered = 0x84
let dw_AT_call_data_location = 0x85
let dw_AT_call_data_value = 0x86
let dw_AT_noreturn = 0x87
let dw_AT_alignment = 0x88
let dw_AT_export_symbols = 0x89
let dw_AT_deleted = 0x8a
let dw_AT_defaulted = 0x8b
let dw_AT_loclists_base = 0x8c
let dw_AT_lo_user = 0x2000
let dw_AT_hi_user = 0x3fff


(* FIXME: we cheat for now *)
let dw_LANG_Swift = 0x001e

end

(* DWARF attributes *)

type dw_AT = Producer of string
           | Language of int
           | Name of string
           | Stmt_list of int
           | Comp_dir of string
           | Low_pc of int
           | High_pc of int

(* DWARF tags *)

type dw_TAG = Compile_unit of string * string (* compilation directory, file name *)
            | Subprogram
            | Formal_parameter
            | Variable
            | Typedef
            | Structure_type
            | Member

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
  let open Dwarf5 in function
  | Producer p -> fakeFile p dw_AT_producer Nop
  | Language l -> fakeColumn l dw_AT_language Nop
  | Name n -> fakeFile n dw_AT_name Nop
  | Stmt_list l -> fakeColumn l dw_AT_stmt_list Nop
  | Comp_dir n -> fakeFile n dw_AT_comp_dir Nop
  | Low_pc l -> fakeColumn l dw_AT_low_pc Nop
  | High_pc h -> fakeColumn h dw_AT_high_pc Nop
  | _ -> assert false

(* emit a DW_TAG
   When it admits children, these follow sequentially,
   closed by dw_tag_children_done.
   Otherwise siblings follow.
 *)

let dw_tag : dw_TAG -> t =
  let open Dwarf5 in function
  | Compile_unit (dir, file) ->
    fakeColumn 0 dw_TAG_compile_unit
      (Block
         ([],
          (dw_attr (Producer "Motoko version 0.1") ^^
           dw_attr (Producer "Motoko compiler version 0.1") ^^
           dw_attr (Language Dwarf5.dw_LANG_Swift) ^^ (* FIXME *)
           dw_attr (Name file) ^^
           dw_attr (Stmt_list 0) ^^ (* FIXME *)
           dw_attr (Comp_dir dir) ^^
           dw_attr (Low_pc 0) ^^
           dw_attr (High_pc 0xFF) (* FIXME *)
          ) 0l Wasm.Source.no_region []))
  | _ -> assert false

let dw_tag_children_done : t =
  block_ [] (fun _ _ x -> (Nop @@ Wasm.Source.no_region) :: x)
