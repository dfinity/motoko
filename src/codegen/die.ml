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

