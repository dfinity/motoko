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

open Mo_types
open Die

(* Note [emit a DW_TAG]
   ~~~~~~~~~~~~~~~~~~~~
   There are two principal types of DWARF tags, those
   which are intimately tied to the generated instructions
   and those that are not. The latter ones include type
   definitions which can be regarded as global, while
   the former bracket or delimit instructions, like
   scoping or variable definitions. These are required to
   float with the instruction stream.

   Another aspect of tags is whether a tag admits children.
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

(*let pointer_key = ref None*)

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

(* injecting a tag into the instruction stream, see Note [emit a DW_TAG] *)
let rec dw_tag_open : dw_TAG -> t =
  let dw_prim_type prim = fst (dw_prim_type_ref prim) in
  let dw_type ty = fst (dw_type_ref ty) in
  let open Type in
  function
  | Compile_unit (dir, file) ->
    let base_types = (* these are emitted for inspectionability *)
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
    dw ^^
    meta_tag Wasm_exts.Abbreviation.dw_TAG_subprogram_Ret
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

and meta_tag tag attrs =
  i (Meta (unreferencable_tag tag attrs))
and metas = concat_map (fun  die -> i (Meta die))
and dw_typedef_ref c ty =
  let ds, r = typedef_ref c ty in
  metas ds, r
and dw_type_ref ty =
  let ds, r = type_ref ty in
  metas ds, r
and dw_prim_type_ref (prim : Type.prim) =
  let ds, r = prim_type_ref prim in
  metas ds, r
and dw_enum vnts =
  let ds, r = enum vnts in
  metas ds ^^ dw_tag_close, r
and dw_option_instance key =
  let ds, r = option_instance key in
  metas ds ^^ dw_tag_close, r
and dw_variant vnts =
  let ds, r = variant vnts in
  metas ds ^^ dw_tag_close, r
and dw_object fs =
  let ds, r = object_ fs in
  metas ds ^^ dw_tag_close, r
and dw_tuple ts =
  let ds, r = tuple ts in
  metas ds ^^ dw_tag_close, r
let dw_tag die body = dw_tag_open die ^^ body ^^ dw_tag_close
let dw_tag_no_children = dw_tag_open (* self-closing *)

(* Marker for statement boundaries *)
let dw_statement { Source.left; Source.right } =
  let open Wasm.Source in
  let left = { file = left.Source.file; line = left.Source.line; column = left.Source.column } in
  i (Meta (StatementDelimiter left))
