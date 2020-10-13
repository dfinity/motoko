open Mo_types
open Wasm_exts.Dwarf5
open Meta

(* Note [Low_pc, High_pc, Ranges are special]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The DWARF attributes `Low_pc`, `High_pc` and `Ranges` carry information
about the Wasm bytecode's layout in the emitted Code section for the
compilation unit. The information is not available here, so these
attributes have no payload at this side. Instead it is filled in
in a tag-dependent manner by `customModuleEncode.ml`. For LexicalBlock
the Low_pc and High_pc attributes are managed entirely by the emitter.
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
           | Const_value of int
           | Discr_value of int
           | Artificial of bool
           | TypeRef of int (* reference *)
           | TypePromise of int Lib.Promise.t (* reference *)
           | Encoding of int
           | Location of int list
           | DataMemberLocation of int

(* DWARF tags *)

type dw_TAG =
  | Compile_unit of string * string                            (* compilation directory, file name *)
  | Subprogram of string * Type.typ list * Source.pos          (* name, return types, location *)
  | LexicalBlock of Source.pos
  | Formal_parameter of (string * Source.pos * Type.typ * int) (* name, location, type, Wasm slot *)
  | Variable of (string * Source.pos * Type.typ * int)         (* name, location, type, Wasm slot *)
  | Type of Type.typ
  | Typedef of string * Type.typ
  (*| Member*)
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
  | TypePromise p ->
    (* See Note [placeholder promises for typedefs] *)
    IntAttribute (dw_AT_type, Wasm_exts.CustomModuleEncode.promise_reference_slot p)
  | Encoding e -> IntAttribute (dw_AT_encoding, e)
  | Discr_value v -> IntAttribute (dw_AT_discr_value, v)
  | Const_value v -> IntAttribute (dw_AT_const_value, v)
  | DataMemberLocation offs -> IntAttribute (dw_AT_data_member_location, offs)
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
      contents buf in
    StringAttribute (dw_AT_location, string_of_ops ops)

let dw_attr at : die list = [dw_attr' at]

let dw_attrs = List.map dw_attr'

open Wasm_exts.Ast

let unreferencable_tag tag attrs =
  Tag (None, tag, attrs)

let with_referencable_meta_tag' f tag attrs : instr' * int =
  let refslot = Wasm_exts.CustomModuleEncode.allocate_reference_slot () in
  f refslot;
  Meta (Tag (Some refslot, tag, attrs)),
  refslot

let with_referencable_meta_tags f tag attrs : instr' list * int =
  let m, r = with_referencable_meta_tag' f tag attrs in
  [m], r

let obvious_prim_of_con c ty : Type.prim option =
  match Type.normalize ty with
  | Type.Prim p ->
    if Arrange_type.(prim p = con c) then Some p else None
  | _ -> None

let is_enum =
  let no_payload = function
    | Type.{typ = Tup []; _} -> true
    | _ -> false in
  List.for_all no_payload

(* Mutable state for the already referencable type DIEs *)
let any_type = ref None
module TypedefRefs = Map.Make (struct type t = Type.kind Con.t let compare = compare end)
let dw_typedefs = ref TypedefRefs.empty
module PrimRefs = Map.Make (struct type t = Type.prim let compare = compare end)
let dw_prims = ref PrimRefs.empty
module EnumRefs = Map.Make (struct type t = string list let compare = compare end)
let dw_enums = ref EnumRefs.empty
module ObjectRefs = Map.Make (struct type t = (string * int) list let compare = compare end)
let dw_objects = ref ObjectRefs.empty
module TupleRefs = Map.Make (struct type t = int list let compare = compare end)
let dw_tuples = ref TupleRefs.empty

(* Factory for referencable type DIEs *)
let rec type_ref : Type.typ -> instr' list * int =
  function
  | Type.Any ->
    begin match !any_type with
    | Some r -> [], r
    | None ->
      let add r = any_type := Some r in
      with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [Name "Any"; Bit_size 0; Data_bit_offset 0; Encoding dw_ATE_address])
    end
  | Type.Prim pr -> prim_type_ref pr
  | Type.Variant vs when is_enum vs -> enum vs
(*  | Type.Variant vs -> variant vs *)
  | Type.(Obj (Object, fs)) -> object_ fs
  | Type.(Tup cs) -> tuple cs
  | Type.Con (c, _) as ty ->
    begin match obvious_prim_of_con c ty with
    | Some p -> type_ref (Type.Prim p)
    | None -> typedef_ref c ty
    end
(*  | Type.Opt inner ->
    let prereq, selector = dw_type_ref inner in
    (* make sure all prerequisite types are around *)
    effects prereq ^^< dw_option_instance selector *)
  | typ -> Printf.printf "Cannot type typ: %s\n" (Wasm.Sexpr.to_string 80 (Arrange_type.typ typ)); type_ref Type.Any (* FIXME assert false *)

and typedef_ref c ty : instr' list * int =
  match TypedefRefs.find_opt c !dw_typedefs with
  | Some r -> ([], r)
  | None ->
    let add r = dw_typedefs := TypedefRefs.add c r !dw_typedefs in
    (* See Note [placeholder promises for typedefs] *)
    let p = Lib.Promise.make () in
    let name = match Arrange_type.con c with | Wasm.Sexpr.Atom n -> n | _ -> assert false in
    let m, typedef_ref = with_referencable_meta_tag' add dw_TAG_typedef (dw_attrs [Name name; TypePromise p]) in
    let ms, reference = type_ref (Type.normalize ty) in
    Lib.Promise.fulfill p reference;
    m :: ms, typedef_ref
and prim_type_ref (prim : Type.prim) : instr' list * int =
  match PrimRefs.find_opt prim !dw_prims with
  | Some r -> [], r
  | None ->
    let name = Name (Type.string_of_prim prim) in
    let add r = dw_prims := PrimRefs.add prim r !dw_prims in
    let ms, r =
      match prim with
      | Type.Bool ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 1; Data_bit_offset 1; Encoding dw_ATE_boolean])
      | Type.Char ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 29; Data_bit_offset 8; Encoding dw_ATE_UTF])
      | Type.(Int | Nat) ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.Text -> (* FIXME: should be dynamic, like Any *)
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_UTF])
      | Type.(Int8|Int16|Int32) ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.(Word8|Nat8|Word16|Nat16|Word32|Nat32) ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_unsigned])
      | Type.Int64 ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 64; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.(Word64|Nat64) ->
        with_referencable_meta_tags add dw_TAG_base_type
          (dw_attrs [name; Bit_size 64; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_unsigned])
      | ty -> (*Printf.eprintf "Cannot type: %s\n" (Wasm.Sexpr.to_string 80 (Arrange_type.prim prim));*) type_ref Type.Any (* FIXME, this is "Any" for now *)
(* | _ -> assert false (* TODO *)*)
    in
    ms, r
and enum vnts : instr' list * int =
  let selectors = List.map (fun Type.{lab; _} -> lab) vnts in
  match EnumRefs.find_opt selectors !dw_enums with
  | Some r -> [], r
  | None ->
    let add r = dw_enums := EnumRefs.add selectors r !dw_enums in
    let enumerator name =
        let hash = Int32.to_int (Mo_types.Hash.hash name) in
        unreferencable_tag dw_TAG_enumerator (dw_attrs [Name name; Const_value hash]) in
    (*  enumeration_type, useful only with location expression *)
    with_referencable_meta_tags add
       dw_TAG_enumeration_type
       (dw_attr' (Artificial true) :: List.map enumerator selectors)



and object_ fs =
  let open List in
  let open Wasm_exts.Abbreviation in
  let selectors = map (fun Type.{lab; typ} -> lab, type_ref typ) fs in
  (* make sure all prerequisite types are around *)
  let prereqs = Lib.List.concat_map (fun (_, (ms, _)) -> ms) selectors in
  let key = map (fun (name, (_, reference)) -> name, reference) selectors in
  match ObjectRefs.find_opt key !dw_objects with
  | Some r -> prereqs, r
  | None ->
    let add r = dw_objects := ObjectRefs.add key r !dw_objects in
    let ms, r =
      let field (name, (_, r)) : die =
        let _hash = Lib.Uint32.to_int (Idllib.IdlHash.idl_hash name) in (* TODO *)
        unreferencable_tag dw_TAG_member_Word_sized_typed
          (dw_attrs [Name name; TypeRef r; Byte_size 4 (*; Location search *)]) in
      (* reference to structure_type *)
      with_referencable_meta_tags add dw_TAG_structure_type
          (dw_attrs [Name "@obj"; Byte_size (4 * length selectors)] @ map field selectors) in
    prereqs @ ms, r



and tuple ts : instr' list * int =
  let open List in
  let open Wasm_exts.Abbreviation in
  let field_types_refs = map type_ref ts in
  let field_refs = map snd field_types_refs in
  let prereqs = Lib.List.concat_map fst field_types_refs in
  match TupleRefs.find_opt field_refs !dw_tuples with
  | Some r -> prereqs, r
  | None ->
    let add r = dw_tuples := TupleRefs.add field_refs r !dw_tuples in
    let ms, r =
      let field index (_, r) =
        unreferencable_tag dw_TAG_member_Word_sized_typed
          (dw_attrs [Name (Printf.sprintf ".%d" index); TypeRef r; Byte_size 4]) in
      with_referencable_meta_tags add dw_TAG_structure_type
        (dw_attrs [Name "@tup"; Byte_size 4] @ mapi field field_types_refs) in
    prereqs @ ms, r
