(*
This module provides some convenience to assemble WASM instruction lists. The
features are

 * O(1) concatenation (using difference list internally)
 * Managing of label depths.
 * Some simple peephole optimizations.
 * Helpers for DWARF elements (tags and attributes).
*)

open Wasm_exts.Ast
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
  let open Wasm_exts.CustomModuleEncode in
  let rec go l r = match l, r with
    (* Combine adjacent Metas *)
    | {it = Meta m2; _} as n2 :: {it = Meta m1; _} :: l', r' ->
      let combine = let open Wasm_exts.Dwarf5.Meta in function
        | StatementDelimiter _, StatementDelimiter _ -> m2
        | StatementDelimiter _, Grouped (StatementDelimiter _ :: t) -> Grouped (m2 :: t)
        | Grouped g1, Grouped g2 -> Grouped (g2 @ g1)
        | Grouped g1, _ -> Grouped (m2 :: g1)
        | _, Grouped g2 -> Grouped (g2 @ [m1])
        | _, _ -> Grouped [m2; m1] in
      go ({ n2 with it = Meta (combine (m1, m2)) } :: l') r'

    (* Loading and dropping is pointless *)
    | { it = Const _ | LocalGet _; _} :: l', { it = Drop; _ } :: r' -> go l' r'
    (* Loading and dropping is pointless, even with intervening Meta *)
    | { it = Meta _; _} as m :: { it = Const _ | LocalGet _; _} :: l', { it = Drop; _ } :: r' -> go l' (m :: r')
    (* The following is not semantics preserving for general Wasm (due to out-of-memory)
       but should be fine for the code that we create *)
    | { it = Load _; _} :: l', { it = Drop; _ } :: _ -> go l' r
    (* Introduce LocalTee *)
    | { it = LocalSet n1; _} :: l', ({ it = LocalGet n2; _ } as i) :: r' when n1 = n2 ->
      go l' ({i with it = LocalTee n2 } :: r')
    (* Introduce LocalTee with previously intervening Meta *)
    | { it = Meta _; _} as m :: { it = LocalSet n1; _} :: l', ({ it = LocalGet n2; _ } as i) :: r' when n1 = n2 ->
      go l' (m :: {i with it = LocalTee n2 } :: r')
    (* Eliminate LocalTee followed by Drop (good for confluence) *)
    | ({ it = LocalTee n; _} as i) :: l', { it = Drop; _ } :: r' ->
      go l' ({i with it = LocalSet n } :: r')
    (* Code after Return, Br or Unreachable is dead *)
    | _, ({ it = Return | Br _ | Unreachable; _ } as i) :: t ->
      (* see Note [funneling DIEs through Wasm.Ast] *)
      List.(rev (i :: l) @ find_all (fun instr -> is_dwarf_like instr.it) t)
    (* `If` blocks after pushed constants are simplifiable *)
    | { it = Const {it = I32 0l; _}; _} :: l', ({it = If (res,_,else_); _} as i) :: r' ->
      go l' ({i with it = Block (res, else_)} :: r')
    | { it = Const {it = I32 _; _}; _} :: l', ({it = If (res,then_,_); _} as i) :: r' ->
      go l' ({i with it = Block (res, then_)} :: r')
    (* `If` blocks after negation can swap legs *)
    | { it = Test (I32 I32Op.Eqz); _} :: l', ({it = If (res,then_,else_); _} as i) :: r' ->
      go l' ({i with it = If (res,else_,then_)} :: r')
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


(* Do nothing *)
let nop : t = fun _ _ rest -> rest

(* The concatenation operator *)
let (^^) (is1 : t) (is2 : t) : t = fun d pos rest -> is1 d pos (is2 d pos rest)

(* Forcing side effects to happen,
   only for depth- and location-oblivious instructions *)
let effects t =
  let instrs = t 0l Wasm.Source.no_region [] in
  fun _ _ rest -> instrs @ rest

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

let as_block_type : stack_type -> block_type = function
  | [] -> ValBlockType None
  | [t] -> ValBlockType (Some t)
  | _ -> raise (Invalid_argument "instrList block combinators do not support multi-value yet")

let if_ (ty : stack_type) (thn : t) (els : t) : t =
  fun d pos rest ->
    (If (as_block_type ty, to_nested_list d pos thn, to_nested_list d pos els) @@ pos) :: rest

let block_ (ty : stack_type) (body : t) : t =
  fun d pos rest ->
    (Block (as_block_type ty, to_nested_list d pos body) @@ pos) :: rest

let loop_ (ty : stack_type) (body : t) : t =
  fun d pos rest ->
    (Loop (as_block_type ty, to_nested_list d pos body) @@ pos) :: rest

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

(* Convenience combinators *)

let labeled_block_ (ty : stack_type) depth (body : t) : t =
  block_ ty (remember_depth depth body)

(* Intended to be used within assert *)

let is_nop (is : t) =
  is 0l Wasm.Source.no_region [] = []

(* DWARF tags and attributes: see Note [funneling DIEs through Wasm.Ast] *)

open Wasm_exts.Dwarf5
open Meta

open Wasm_exts.Abbreviation
open Mo_types

(* Note [Low_pc, High_pc, Ranges are special]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The DWARF attributes `Low_pc`, `High_pc` and `Ranges` carry information
about the Wasm bytecode's layout in the emitted Code section for the
compilation unit. The information is not available here, so these
attributes have no payload at this side. Instead it is filled in
in a tag-dependent manner by the emitting module. For LexicalBlock
the Low_pc and High_pc attributed are managed entirely by the emitter.
 *)

type dw_AT = Producer of string
           | Language of int
           | Name of string
           | Stmt_list of int
           | Comp_dir of string
           | Use_UTF8 of bool
           | Low_pc | High_pc | Ranges (* see Note [Low_pc, High_pc, Ranges are special] *)
           | Addr_base of int
           | Decl_file of string
           | Decl_line of int
           | Decl_column of int
           | Prototyped of bool
           | External of bool
           | Byte_size of int
           | Bit_size of int
           | Data_bit_offset of int
           | Discr of int (* reference *)
           | Discr_list
           | Const_value of int
           | Discr_value of int
           | Artificial of bool
           | TypeRef of int (* reference *)
           | Encoding of int
           | Location of int list

(* DWARF tags *)

type dw_TAG =
  | Compile_unit of string * string (* compilation directory, file name *)
  | Subprogram of string * Source.pos
  | LexicalBlock of Source.pos
  | Formal_parameter of (string * Source.pos * Type.typ * int)
  | Variable of (string * Source.pos * Type.typ * int)
  | Type of Type.typ
  | Typedef of string * Type.typ
  | Pointer_type of int (* needed? *)
  | Structure_type of int (* needed? *)
  | Member
  | Variant_part
  | Variant

(* DWARF high-level structures *)

let dw_attr' : dw_AT -> die =
  let bool b = if b then 1 else 0 in
  function
  | Producer p -> StringAttribute (dw_AT_producer, p)
  | Language l -> IntAttribute (dw_AT_language, l)
  | Name n -> StringAttribute (dw_AT_name, n)
  | Stmt_list l -> IntAttribute (dw_AT_stmt_list, l)
  | Comp_dir d -> StringAttribute (dw_AT_comp_dir, d)
  | Use_UTF8 b -> IntAttribute (dw_AT_use_UTF8, bool b)
  | Addr_base b -> IntAttribute (dw_AT_addr_base, b)
  | Low_pc -> OffsetAttribute dw_AT_low_pc
  | High_pc -> OffsetAttribute dw_AT_high_pc
  | Ranges -> OffsetAttribute dw_AT_ranges  (* see Note [Low_pc, High_pc, Ranges are special] *)
  | Decl_file f -> StringAttribute (dw_AT_decl_file, f)
  | Decl_line l -> IntAttribute (dw_AT_decl_line, l)
  | Decl_column c -> IntAttribute (dw_AT_decl_column, c)
  | Prototyped b -> IntAttribute (dw_AT_prototyped, bool b)
  | External b -> IntAttribute (dw_AT_external, bool b)
  | Byte_size s -> IntAttribute (dw_AT_byte_size, s)
  | Bit_size s -> IntAttribute (dw_AT_bit_size, s)
  | Data_bit_offset o -> IntAttribute (dw_AT_data_bit_offset, o)
  | Artificial b -> IntAttribute (dw_AT_artificial, bool b)
  | Discr r -> IntAttribute (dw_AT_discr, r)
  | TypeRef r -> IntAttribute (dw_AT_type, r)
  | Encoding e -> IntAttribute (dw_AT_encoding, e)
  | Discr_value v -> IntAttribute (dw_AT_discr_value, v)
  | Const_value v -> IntAttribute (dw_AT_const_value, v)
  | Location ops ->
    let string_of_ops ops =
      let open Buffer in
      let buf = create 16 in
      let rec stash = function
        | i when i >= 0 -> assert (i < 0x100); add_char buf (Char.chr i)
        | i when -i < 128 -> stash (-i) (* ULEB128 byte *)
        | i -> (* needs ULEB128 chopping *)
          let i = -i in
          stash (i land 0x7F lor 0x80);
          stash (- (i lsr 7)) in
      List.iter stash ops;
      Buffer.contents buf in
    StringAttribute (dw_AT_location, (string_of_ops ops))
  | Discr_list -> assert false (* not yet *)

let dw_attr at : die list = [dw_attr' at]

let dw_attrs = List.map dw_attr'

(* Note [emit a DW_TAG]
   ~~~~~~~~~~~~~~~~~~~~
   When it admits children, these follow sequentially,
   closed by dw_tag_close. The abbreviation table must
   be consulted when deciding between
   - dw_tag_no_children, or
   - dw_tag.
   The former is self-closing (no nested tags), and the
   latter is high-level, straddling a region of code.
   The low-level alternative to the latter is using the
   dw_tag_open/dw_tag_close pair explicitly to demarcate
   the enclosed instructions. This moves the burden of
   balancing to the user.
   See also: Note [funneling DIEs through Wasm.Ast]
 *)

let dw_tag_close : t =
  i (Meta TagClose)

module PrimRefs = Map.Make (struct type t = Type.prim let compare = compare end)
let dw_prims = ref PrimRefs.empty
module EnumRefs = Map.Make (struct type t = string list let compare = compare end) (* FIXME: consider types *)
let dw_enums = ref EnumRefs.empty
module ObjectRefs = Map.Make (struct type t = string list let compare = compare end) (* FIXME: consider types *)
let dw_objects = ref ObjectRefs.empty
let any_type = ref None

let pointer_key = ref None

(* injecting a tag into the instruction stream, see Note [emit a DW_TAG] *)
let rec dw_tag_open : dw_TAG -> t =
  function
  | Compile_unit (dir, file) ->
    let base_types =
      dw_prim_type Type.Bool ^^
      dw_prim_type Type.Char ^^
        (*dw_prim_type Type.Text ^^*)
      dw_prim_type Type.Word8 ^^
      dw_prim_type Type.Nat8 ^^
      dw_prim_type Type.Int8 ^^
      dw_prim_type Type.Word16 ^^
      dw_prim_type Type.Nat16 ^^
      dw_prim_type Type.Int16 ^^
      dw_prim_type Type.Word32 ^^
      dw_prim_type Type.Nat32 ^^
      dw_prim_type Type.Int32 (* ^^
      dw_prim_type Type.Word64 ^^
      dw_prim_type Type.Nat64 ^^
      dw_prim_type Type.Int64 *)
    in
    let builtin_types =
      dw_type Type.Any ^^
      dw_prim_type Type.Nat ^^
      dw_prim_type Type.Int
    in
    meta_tag dw_TAG_compile_unit
      (dw_attrs
         [ Producer (Printf.sprintf "DFINITY Motoko compiler, revision %s" Source_id.id);
           Language dw_LANG_C99; Name file; Stmt_list 0;
           Comp_dir dir; Use_UTF8 true; Low_pc; Addr_base 8; Ranges ]) ^^
      base_types ^^
      builtin_types
  | Subprogram (name, pos) ->
    meta_tag dw_TAG_subprogram
      (dw_attrs [Low_pc; High_pc; Name name; Decl_file pos.Source.file; Decl_line pos.Source.line; Decl_column pos.Source.column; Prototyped true; External false])
  | Formal_parameter (name, pos, ty, slot) ->
    let dw, reference = dw_type_ref ty in
    dw ^^
    meta_tag dw_TAG_formal_parameter
      (dw_attrs [Name name; Decl_line pos.Source.line; Decl_column pos.Source.column; TypeRef reference; Location (Location.local slot [ dw_OP_stack_value ])])
  | LexicalBlock pos ->
    meta_tag dw_TAG_lexical_block
      (dw_attrs [Decl_line pos.Source.line; Decl_column pos.Source.column])
  | Variable (name, pos, ty, slot) ->
    let dw, reference = dw_type_ref ty in
    dw ^^
    meta_tag dw_TAG_variable
      (dw_attrs [Name name; Decl_line pos.Source.line; Decl_column pos.Source.column; TypeRef reference; Location (Location.local slot [ dw_OP_stack_value ])])
  | Type ty -> dw_type ty
  | _ -> assert false
and lookup_pointer_key () : t * int =
  match !pointer_key with
  | Some r -> nop, r
  | None ->
    let dw, r =
      referencable_meta_tag (assert (dw_TAG_base_type_Anon > dw_TAG_base_type); dw_TAG_base_type_Anon)
        (dw_attrs [Bit_size 1; Data_bit_offset 1]) in
    pointer_key := Some r;
    dw, r
and meta_tag tag attrs =
  i (Meta (Tag (None, tag, attrs)))
and referencable_meta_tag tag attrs : t * int =
  let refslot = Wasm_exts.CustomModuleEncode.allocate_reference_slot () in
  i (Meta (Tag (Some refslot, tag, attrs))),
  refslot
and dw_type ty = fst (dw_type_ref ty)
and dw_type_ref =
  function
  | Type.Any ->
    begin match !any_type with
    | Some reference -> nop, reference
    | None ->
      let dw, reference =
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [Name "Any"; Bit_size 0; Data_bit_offset 0; Encoding dw_ATE_address]) in
      any_type := Some reference;
      dw, reference
    end
  | Type.Prim pr -> dw_prim_type_ref pr
  | Type.Variant vs when is_enum vs -> dw_enum vs
  | Type.(Obj (Object, fs)) -> dw_object fs

  (* | Type.Opt inner -> assert false templated type *)
  | typ -> (*Printf.printf "Cannot type typ: %s\n" (Wasm.Sexpr.to_string 80 (Arrange_type.typ typ));*) dw_type_ref Type.Any (* FIXME assert false *)

and (^^<) dw1 (dw2, r) = (dw1 ^^ dw2, r)
and (^<^) (dw1, r) dw2 = (dw1 ^^ dw2, r)
and dw_prim_type prim = fst (dw_prim_type_ref prim)
and dw_prim_type_ref (prim : Type.prim) =
  match PrimRefs.find_opt prim !dw_prims with
  | Some r -> nop, r
  | None ->
    let name = Name (Type.string_of_prim prim) in
    let dw, refindx =
      match prim with
      | Type.Bool ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 1; Data_bit_offset 0; Encoding dw_ATE_boolean])
      | Type.Char ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 29; Data_bit_offset 8; Encoding dw_ATE_UTF])
      | Type.(Word8 | Nat8 | Int8) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 24; Encoding dw_ATE_unsigned])
      | Type.(Word16 | Nat16 | Int16) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 16; Encoding dw_ATE_unsigned])
      | Type.(Int | Nat) ->
        let pointer_key_dw, pointer_key = lookup_pointer_key () in
        let struct_dw, struct_ref = referencable_meta_tag dw_TAG_structure_type
          (dw_attrs [name; Byte_size 4]) in
        let mark_dw, mark = referencable_meta_tag dw_TAG_member_Pointer_mark
          (dw_attrs [Name "@pointer_mark"; TypeRef pointer_key; Artificial true; Bit_size 1; Data_bit_offset 1]) in
        pointer_key_dw ^^
        struct_dw ^^
        mark_dw ^^
        meta_tag dw_TAG_variant_part
          (dw_attr (Discr mark)) ^^
        dw_tag_close ^^ (* closing dw_TAG_variant_part *)
        dw_tag_close,  (* closing dw_TAG_structure_type *)
        struct_ref
      | Type.Word32 ->
        let internalU30 =
          referencable_meta_tag dw_TAG_base_type_Unsigned_Anon
            (dw_attrs [Bit_size 30; Data_bit_offset 2; Encoding dw_ATE_unsigned]) in
        let internalU32_dw, internalU32 =
          referencable_meta_tag dw_TAG_base_type_Unsigned_Bytes_Anon
            (dw_attrs [Byte_size 4; Encoding dw_ATE_unsigned]) in
        let pointedU32 =
          internalU32_dw ^^<
          referencable_meta_tag dw_TAG_pointer_type
            (dw_attr (TypeRef internalU32)) in
        let pointer_key_dw, pointer_key = lookup_pointer_key () in
        let flag_member_dw, flag_member =
          pointer_key_dw ^^<
          referencable_meta_tag dw_TAG_member_Pointer_mark
            (dw_attrs [Name "@pointer_mark"; TypeRef pointer_key; Artificial true; Bit_size 1; Data_bit_offset 1]) in
        let variant_part =
          flag_member_dw ^^
          meta_tag dw_TAG_variant_part
            (dw_attr (Discr flag_member)) ^^
          meta_tag dw_TAG_variant
            (dw_attr (Discr_value 0)) ^^
          meta_tag dw_TAG_member_Pointer_mark (* FIXME *)
            (dw_attrs [Name "@non-pointer"; TypeRef (snd internalU30); Artificial true; Bit_size 30; Data_bit_offset 2]) ^^
          dw_tag_close ^^ (* variant 0 *)
          meta_tag dw_TAG_variant
            (dw_attr (Discr_value 1)) ^^
          meta_tag dw_TAG_member_Pointer_mark (* FIXME *)
            (dw_attrs [Name "@pointer"; TypeRef (snd pointedU32); Artificial true; Bit_size 32; Data_bit_offset 0]) ^^
          dw_tag_close ^^ (* variant 1 *)
          dw_tag_close (* variant part *)
        in

(*
<3><444>: Abbrev Number: 6 (DW_TAG_structure_type)
   <445>   DW_AT_name        : (indirect string, offset: 0xa7f7f): Option<&u8>
   <449>   DW_AT_byte_size   : 8
   <44a>   DW_AT_alignment   : 8
      <4><44b>: Abbrev Number: 9 (DW_TAG_member)
         <44c>   DW_AT_type        : <0x509>
         <450>   DW_AT_alignment   : 8
         <451>   DW_AT_data_member_location: 0
         <452>   DW_AT_artificial  : 1
      <4><452>: Abbrev Number: 10 (DW_TAG_variant_part)
         <453>   DW_AT_discr       : <0x44b>
            <5><457>: Abbrev Number: 11 (DW_TAG_variant)
               <458>   DW_AT_discr_value : 0
                  <6><459>: Abbrev Number: 12 (DW_TAG_memberrefindx)
                     <45a>   DW_AT_type        : <0x46b>
                     <45e>   DW_AT_alignment   : 8
                     <45f>   DW_AT_data_member_location: 0
                  <6><460>: Abbrev Number: 0
            <5><461>: Abbrev Number: 13 (DW_TAG_variant)
                  <6><462>: Abbrev Number: 12 (DW_TAG_member)
                     <463>   DW_AT_type        : <0x472>
                     <467>   DW_AT_alignment   : 8
                     <468>   DW_AT_data_member_location: 0

 *)

        (fst pointedU32 ^^ fst internalU30) ^^<
        referencable_meta_tag dw_TAG_structure_type
          (dw_attrs [name; Byte_size 4]) ^<^
          variant_part ^^
          dw_tag_close
      (*  dw_tag (Variant_part (pointer_key, [Variant internalU30, Variant pointedU32])) *)
      | ty -> (*Printf.eprintf "Cannot type: %s\n" (Wasm.Sexpr.to_string 80 (Arrange_type.prim prim));*) dw_type_ref Type.Any (* FIXME, this is "Any" for now *)
(* | _ -> assert false (* TODO *)*)
    in
    dw_prims := PrimRefs.add prim refindx !dw_prims;
    dw, refindx
and is_enum =
  let no_payload = function
    | Type.{typ = Tup []; _} -> true
    | _ -> false in
  List.for_all no_payload
and dw_enum vnts =
  let selectors = List.map (fun Type.{lab; _} -> lab) vnts in
  match EnumRefs.find_opt selectors !dw_enums with
  | Some r -> nop, r
  | None ->
    let enum_ref =
      (* reference to enumeration_type *)
      let internal_enum =
        referencable_meta_tag dw_TAG_enumeration_type (dw_attr (Artificial true)) in
      let enumerator name =
        let hash = Lib.Uint32.to_int (Idllib.IdlHash.idl_hash name) in
        meta_tag dw_TAG_enumerator (dw_attrs [Name name; Const_value hash]) in
      (fst internal_enum ^^
       concat_map enumerator selectors ^^
       dw_tag_close (* enumeration_type *)) ^^<
      referencable_meta_tag dw_TAG_reference_type
        (dw_attr (TypeRef (snd internal_enum))) in
    dw_enums := EnumRefs.add selectors (snd enum_ref) !dw_enums;
    enum_ref
and dw_object fs =
  let selectors = List.map (fun Type.{lab; _} -> lab) fs in
  match ObjectRefs.find_opt selectors !dw_objects with
  | Some r -> nop, r
  | None ->
    let struct_ref =
      (* reference to structure_type *)
      let internal_struct =
        referencable_meta_tag dw_TAG_structure_type (dw_attrs [Name "@obj"; Byte_size 4]) in
      let field name =
        let _hash = Lib.Uint32.to_int (Idllib.IdlHash.idl_hash name) in (* TODO *)
        meta_tag dw_TAG_member_Word_sized (dw_attrs [Name name; Byte_size 4]) in
      (fst internal_struct ^^
       concat_map field selectors ^^
       dw_tag_close (* structure_type *)) ^^<
      referencable_meta_tag dw_TAG_reference_type
        (dw_attr (TypeRef (snd internal_struct))) in
    dw_objects := ObjectRefs.add selectors (snd struct_ref) !dw_objects;
    struct_ref

let dw_tag die body = dw_tag_open die ^^ body ^^ dw_tag_close
let dw_tag_no_children = dw_tag_open (* self-closing *)

(* Marker for statement boundaries *)
let dw_statement { Source.left; Source.right } =
  let open Wasm.Source in
  let left = { file = left.Source.file; line = left.Source.line; column = left.Source.column } in
  i (Meta (StatementDelimiter left))
