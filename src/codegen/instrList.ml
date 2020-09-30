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
      List.(rev (i :: l) @ find_all (fun instr -> Wasm_exts.Ast.is_dwarf_like instr.it) t)
    (* Equals zero has an dedicated operation (and works well with leg swapping) *)
    | ({it = Compare (I32 I32Op.Eq); _} as i) :: {it = Const {it = I32 0l; _}; _} :: l', r' ->
      go l' ({ i with it = Test (I32 I32Op.Eqz)}  :: r')
    (* eqz after eq/ne becomes ne/eq *)
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I32 I32Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (I32 I32Op.Ne)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I32 I32Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (I32 I32Op.Eq)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I64 I64Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (I64 I64Op.Ne)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I64 I64Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (I64 I64Op.Eq)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F32 F32Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (F32 F32Op.Ne)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F32 F32Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (F32 F32Op.Eq)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F64 F64Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (F64 F64Op.Ne)}  :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F64 F64Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (F64 F64Op.Eq)}  :: r')
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
module TypedefRefs = Map.Make (struct type t = Type.kind Con.t let compare = compare end)
let dw_typedefs = ref TypedefRefs.empty
module VariantRefs = Map.Make (struct type t = string list let compare = compare end) (* FIXME: consider types *)
let dw_variants = ref VariantRefs.empty
module EnumRefs = Map.Make (struct type t = string list let compare = compare end) (* FIXME: consider types *)
let dw_enums = ref EnumRefs.empty
module ObjectRefs = Map.Make (struct type t = string list let compare = compare end) (* FIXME: consider types *)
let dw_objects = ref ObjectRefs.empty
module TupleRefs = Map.Make (struct type t = int list let compare = compare end)
let dw_tuples = ref TupleRefs.empty
let any_type = ref None

let pointer_key = ref None


(* Note [locations for types]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   Motoko uses a variety of formats to store data depending on its type
   and even the actual value. Location  expressions are necessary to teach
   the debugger where the data can be found.
   - For short numeric types we have a location expression that LSB-aligns
     and leaves the value on the expression stack. This is a workaround for
     `lldb`'s insufficiency to understand basic types that are bitfields
     (http://lists.llvm.org/pipermail/lldb-dev/2020-August/016393.html).
     This also implies that setting of such variables in `lldb` won't work.
   - 32/64 bit-sized integrals may be stored on the heap, we use `DW_OP_bra`
     to guide the debugger after examining the indirection bit.
     We can't use the pointer key to be a discriminator for DW_TAG_variant_part
     because of lldb (link above), and probably displaying issues.
   - For arbitrary-precision integrals, we cannot inspect the multi-precision
     heap representation yet
   - Variants with no payload map to C-like `enum`s, the location expression
     takes care of focussing on the hash value
   - Variants map to `DW_TAG_variant_*` directly
   - Tuples may use Rust-like decoding
   - Objects need field search for members (when encoded as structure members)
   - The `Any` type will need fully dynamic resolution by `lldb`
   - Parameter types for polymorphic types/functions will be treated as `Any`.
   - `Text` will be treated as `Any` as it needs pretty-printing is the presence
     of concatenation nodes

 *)

let obvious_prim_of_con c ty : Type.prim option =
  match Type.normalize ty with
  | Type.Prim p ->
    if Arrange_type.(prim p = con c) then Some p else None
  | _ -> None

(* injecting a tag into the instruction stream, see Note [emit a DW_TAG] *)
let rec dw_tag_open : dw_TAG -> t =
  let unskew, past_tag = 1, 4 in
  let open Type in
  let rec loc slot = function (* See Note [locations for types] *)
    | Type.Variant vs when is_enum vs -> Location.local slot [ dw_OP_plus_uconst; unskew + past_tag; dw_OP_deref; dw_OP_stack_value ]
    | Type.Variant _ -> Location.local slot [ dw_OP_plus_uconst; unskew ]
    | Prim Text -> Location.local slot [ dw_OP_plus_uconst; unskew; dw_OP_stack_value ]
    | Prim Char -> Location.local slot [ dw_OP_lit8; dw_OP_shr; dw_OP_stack_value ]
    | Prim Bool -> Location.local slot [ dw_OP_lit1; dw_OP_shr; dw_OP_stack_value ]
    | Prim Int8 -> Location.local slot [ dw_OP_lit24; dw_OP_shra; dw_OP_stack_value ]
    | Prim (Word8|Nat8) -> Location.local slot [ dw_OP_lit24; dw_OP_shr; dw_OP_stack_value ]
    | Prim Int16 -> Location.local slot [ dw_OP_lit16; dw_OP_shra; dw_OP_stack_value ]
    | Prim (Word16|Nat16) -> Location.local slot [ dw_OP_lit16; dw_OP_shr; dw_OP_stack_value ]
    | Prim Int32 -> Location.local slot [ dw_OP_dup; dw_OP_lit1; dw_OP_and; dw_OP_bra; 5; 0;
                                          dw_OP_lit1; dw_OP_shra; dw_OP_skip; 3; 0;
                                          dw_OP_plus_uconst; unskew + past_tag; dw_OP_deref; dw_OP_stack_value ]
    | Prim (Word32|Nat32) -> Location.local slot [ dw_OP_dup; dw_OP_lit1; dw_OP_and; dw_OP_bra; 5; 0;
                                                   dw_OP_lit1; dw_OP_shr; dw_OP_skip; 3; 0;
                                                   dw_OP_plus_uconst; unskew + past_tag; dw_OP_deref; dw_OP_stack_value ]
    (* FIXME: for Int64|Word64|Nat64|Nat|Int the heap check is ignored for now *)
    | Prim Int64 -> Location.local slot [ dw_OP_lit1; dw_OP_shra; dw_OP_const4u; 0xFF; 0xFF; 0xFF; 0xFF; dw_OP_and; dw_OP_stack_value ]
    | Prim (Word64|Nat64) -> Location.local slot [ dw_OP_lit1; dw_OP_shr; dw_OP_const4u; 0x7F; 0xFF; 0xFF; 0xFF; dw_OP_and; dw_OP_stack_value ]
    | Prim (Nat|Int) -> Location.local slot [ dw_OP_lit1; dw_OP_shra; dw_OP_stack_value ]

    | Tup _ -> Location.local slot []
    | Con (c, _) as ty ->
      begin match obvious_prim_of_con c ty with
      | Some p -> loc slot (Prim p)
      | _ -> Location.local slot [ dw_OP_stack_value ] (* FIXME: locate real type *)
      end
    | _ -> Location.local slot [ dw_OP_stack_value ] in (* FIXME: objects, options *)
  function
  | Compile_unit (dir, file) ->
    let base_types =
      dw_prim_type Bool ^^
      dw_prim_type Char ^^
      dw_prim_type Text ^^
      dw_prim_type Word8 ^^
      dw_prim_type Nat8 ^^
      dw_prim_type Int8 ^^
      dw_prim_type Word16 ^^
      dw_prim_type Nat16 ^^
      dw_prim_type Int16 ^^
      dw_prim_type Word32 ^^
      dw_prim_type Nat32 ^^
      dw_prim_type Int32 ^^
      dw_prim_type Word64 ^^
      dw_prim_type Nat64 ^^
      dw_prim_type Int64
    in
    let builtin_types =
      dw_type Any ^^
      dw_prim_type Nat ^^
      dw_prim_type Int
    in
    meta_tag dw_TAG_compile_unit
      (dw_attrs
         [ Producer (Printf.sprintf "DFINITY Motoko compiler, revision %s" Source_id.id);
           Language dw_LANG_Motoko; Name file; Stmt_list 0;
           Comp_dir dir; Use_UTF8 true; Low_pc; Addr_base 8; Ranges ]) ^^
      base_types ^^
      builtin_types
  | Subprogram (name, [retty], pos) ->
    let dw, ref_ret = dw_type_ref retty in
    meta_tag dw_TAG_subprogram_Ret
      (dw_attrs [Low_pc; High_pc; Name name; TypeRef ref_ret; Decl_file pos.Source.file; Decl_line pos.Source.line; Decl_column pos.Source.column; Prototyped true; External false])
  | Subprogram (name, _, pos) ->
    meta_tag dw_TAG_subprogram
      (dw_attrs [Low_pc; High_pc; Name name; Decl_file pos.Source.file; Decl_line pos.Source.line; Decl_column pos.Source.column; Prototyped true; External false])
  | Formal_parameter (name, pos, ty, slot) ->
    let dw, reference = dw_type_ref ty in
    dw ^^
    meta_tag dw_TAG_formal_parameter
      (dw_attrs [Name name; Decl_line pos.Source.line; Decl_column pos.Source.column; TypeRef reference; Location (loc slot ty)])
  | LexicalBlock pos ->
    meta_tag dw_TAG_lexical_block
      (dw_attrs [Decl_line pos.Source.line; Decl_column pos.Source.column])
  | Variable (name, pos, ty, slot) ->
    let dw, reference = dw_type_ref ty in
    dw ^^
    meta_tag dw_TAG_variable
      (dw_attrs [Name name; Decl_line pos.Source.line; Decl_column pos.Source.column; TypeRef reference; Location (loc slot ty)])
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
  with_referencable_meta_tag ignore tag attrs
and with_referencable_meta_tag f tag attrs : t * int =
  let refslot = Wasm_exts.CustomModuleEncode.allocate_reference_slot () in
  f refslot;
  i (Meta (Tag (Some refslot, tag, attrs))),
  refslot
and dw_typedef_ref c ty =
  match TypedefRefs.find_opt c !dw_typedefs with
  | Some r -> nop, r
  | None ->
    let add r = dw_typedefs := TypedefRefs.add c r !dw_typedefs in
    (* See Note [placeholder promises for typedefs] *)
    let p = Lib.Promise.make () in
    let name = match Arrange_type.con c with | Wasm.Sexpr.Atom n -> n | _ -> assert false in
    let td = with_referencable_meta_tag add dw_TAG_typedef (dw_attrs [Name name; TypePromise p]) in
    let dw, reference = dw_type_ref (Type.normalize ty) in
    Lib.Promise.fulfill p reference;
    td ^<^ dw
and dw_type ty = fst (dw_type_ref ty)
and dw_type_ref =
  function
  | Type.Any ->
    begin match !any_type with
    | Some r -> nop, r
    | None ->
      let add r = any_type := Some r in
      with_referencable_meta_tag add dw_TAG_base_type
        (dw_attrs [Name "Any"; Bit_size 0; Data_bit_offset 0; Encoding dw_ATE_address])
    end
  | Type.Prim pr -> dw_prim_type_ref pr
  | Type.Variant vs when is_enum vs -> dw_enum vs
  | Type.Variant vs -> dw_variant vs
  | Type.(Obj (Object, fs)) -> dw_object fs
  | Type.(Tup cs) -> dw_tuple cs
  | Type.Con (c, _) as ty ->
    begin match obvious_prim_of_con c ty with
    | Some p -> dw_type_ref (Type.Prim p)
    | None -> dw_typedef_ref c ty
    end
  (* | Type.Opt inner -> assert false templated type *)
  | typ -> Printf.printf "Cannot type typ: %s\n" (Wasm.Sexpr.to_string 80 (Arrange_type.typ typ)); dw_type_ref Type.Any (* FIXME assert false *)

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
          (dw_attrs [name; Bit_size 1; Data_bit_offset 1; Encoding dw_ATE_boolean])
      | Type.Char ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 29; Data_bit_offset 8; Encoding dw_ATE_UTF])
      | Type.(Int | Nat) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.Text -> (* FIXME: should be dynamic, like Any *)
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_UTF])
      | Type.(Int8|Int16|Int32) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.(Word8|Nat8|Word16|Nat16|Word32|Nat32) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 32; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_unsigned])
      | Type.Int64 ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 64; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_signed])
      | Type.(Word64|Nat64) ->
        referencable_meta_tag dw_TAG_base_type
          (dw_attrs [name; Bit_size 64; Data_bit_offset 0(*FIXME: for now*); Encoding dw_ATE_unsigned])
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
    let enum =
      (*  enumeration_type, useful only with location expression *)
      let internal_enum =
        referencable_meta_tag dw_TAG_enumeration_type (dw_attr (Artificial true)) in
      let enumerator name =
        let hash = Int32.to_int (Mo_types.Hash.hash name) in
        meta_tag dw_TAG_enumerator (dw_attrs [Name name; Const_value hash]) in
      internal_enum ^<^
        (concat_map enumerator selectors ^^
         dw_tag_close (* enumeration_type *)) in
    dw_enums := EnumRefs.add selectors (snd enum) !dw_enums;
    enum
and dw_variant vnts =
  let selectors = List.map (fun Type.{lab; typ} -> lab, typ) vnts in
  match VariantRefs.find_opt (List.map fst selectors) !dw_variants with
  | Some r -> nop, r
  | None ->
    let add r = dw_variants := VariantRefs.add (List.map fst selectors) r !dw_variants in
    let prereq (name, typ) =
      let dw_payload_pre, dw_payload_mem =
        match typ with
        | Type.Tup [] -> nop, nop
        | _ ->
          let dw, r = dw_type_ref typ in
          dw,
          meta_tag dw_TAG_member_In_variant (dw_attrs [Name ("#" ^ name); TypeRef r; DataMemberLocation 8]) in
      let dw_overlay, ref_overlay =
        dw_payload_pre ^^<
        (referencable_meta_tag dw_TAG_structure_type (dw_attrs [Name name; Byte_size 12 (*; Artificial *)]) ^<^
         dw_payload_mem ^^
         dw_tag_close (* structure_type *)) in
    (dw_overlay,
     meta_tag dw_TAG_member_In_variant
       (dw_attrs [Name name; TypeRef ref_overlay; DataMemberLocation 8]))  in
    (* make sure all prerequisite types are around *)
    let overlays = List.map prereq selectors in
    let prereqs = effects (concat_map fst overlays) in
    let variant =
      (* struct_type, assumes location points at heap tag *)
      let internal_struct =
        with_referencable_meta_tag add dw_TAG_structure_type (dw_attrs [Name "VARIANT"; Byte_size 8]) in
      let summand (name, mem) =
        let hash = Int32.to_int (Mo_types.Hash.hash name) in
        meta_tag dw_TAG_variant_Named (dw_attrs [Name name; Discr_value hash]) ^^
        mem ^^
        dw_tag_close (* variant *) in
      prereqs ^^<
      internal_struct ^<^
        (meta_tag dw_TAG_member_Tag_mark (dw_attrs [Artificial true; Byte_size 4]) ^^
         let dw2, discr = referencable_meta_tag dw_TAG_member_Variant_mark (dw_attrs [Artificial true; Byte_size 4; DataMemberLocation 4]) in
         dw2 ^^
         (meta_tag dw_TAG_variant_part (dw_attrs [Discr discr])) ^^
         concat_map summand (List.map2 (fun (name, _) (_, mem) -> name, mem) selectors overlays) ^^
         dw_tag_close (* variant_part *) ^^
         dw_tag_close (* struct_type *)) in
    variant
and dw_object fs =
  let selectors = List.map (fun Type.{lab; _} -> lab) fs in
  match ObjectRefs.find_opt selectors !dw_objects with
  | Some r -> nop, r
  | None ->
    let add r = dw_objects := ObjectRefs.add selectors r !dw_objects in
    let struct_ref =
      (* reference to structure_type *)
      let internal_struct =
        with_referencable_meta_tag add dw_TAG_structure_type (dw_attrs [Name "@obj"; Byte_size (4 * List.length selectors)]) in
      let field name =
        let _hash = Lib.Uint32.to_int (Idllib.IdlHash.idl_hash name) in (* TODO *)
        meta_tag dw_TAG_member_Word_sized (dw_attrs [Name name; Byte_size 4]) in
      fst internal_struct ^^
      concat_map field selectors ^^
      dw_tag_close (* structure_type *)
      , snd internal_struct in
    struct_ref
and dw_tuple ts =
  let field_dw_refs = List.map dw_type_ref ts in
  let field_refs = List.map snd field_dw_refs in
  match TupleRefs.find_opt field_refs !dw_tuples with
  | Some r -> nop, r
  | None ->
    let add r = dw_tuples := TupleRefs.add field_refs r !dw_tuples in
    let tuple_type =
      let field index (_, r) =
        meta_tag dw_TAG_member_Word_sized_typed (dw_attrs [Name (Printf.sprintf ".%d" index); TypeRef r; Byte_size 4]) in
      with_referencable_meta_tag add dw_TAG_structure_type (dw_attrs [Name "@tup"; Byte_size 4]) ^<^
      (concat_mapi field field_dw_refs ^^ dw_tag_close (* structure_type *)) in
    concat_map fst field_dw_refs ^^<
    tuple_type

let dw_tag die body = dw_tag_open die ^^ body ^^ dw_tag_close
let dw_tag_no_children = dw_tag_open (* self-closing *)

(* Marker for statement boundaries *)
let dw_statement { Source.left; Source.right } =
  let open Wasm.Source in
  let left = { file = left.Source.file; line = left.Source.line; column = left.Source.column } in
  i (Meta (StatementDelimiter left))
