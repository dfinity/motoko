(*
The type for a dynamic library: A normal WebAssembly module
plus the dylink section.
*)

open Wasm_exts.Ast
open Wasm.Source
open Wasm_exts.CustomModule

(*
This module is a first stab that should be functionally working, but will go
through further refactoring before we are happy with it. Things to do:

 * much code (finding imports, counting) is duplicated for globals and
   functions. This could be refactored into general functions and predicates.
 * There are multiple AST traversals. These could be turned into a single one
   (taking multiple rename functions) or even more generally taking a record
   of functions for each syntactic category.
*)

(*
Resolving GOT.func and GOT.mem imports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GOT.func and GOT.mem imports arise from function and data pointers,
respectively, in languages with pointers (e.g. C and Rust). The idea is that if
a shared library exposes a function/data pointer the entire process should use
the same pointer for the function/data so that the pointer arithmetic and
comparisons will work. For example, this C code:

    __attribute__ ((visibility("default")))
    int f0(int x, int y)
    {
        return x + y;
    }

    __attribute__ ((visibility("default")))
    int (\*f1(void)) (int x, int y)
    {
        return &f0;
    }

generates this GOT.func import:

    (import "GOT.func" "f0" (global (;N;) (mut i32)))

The host is responsible of allocating a table index for this function and
resolving the import to the table index for `f0` so that this code in the
importing module would work:

    assert(f1() == f0);

Note that the definition of `f1` is in the *imported* module and this assertion
is in the *importing* module.

Similarly exposing a data pointer generates a GOT.mem import. All GOT.mem
imports to a symbol should resolve to the same constant to support equality as
above, and additionally pointer arithmetic.

(Pointer arithmetic on function pointers are undefined behavior is C and is not
supported by clang's wasm backend)

Normally this stuff is for dynamic linking, but we want to link the RTS
statically, so we resolve these imports during linking. Currently we only
support GOT.func imports, but implementing GOT.mem imports would be similar.
Secondly, we only support GOT.func imports in the module that defines the
function that we take the address of. This currently works as moc-generated code
doesn't import function addresses from the RTS.

We resolve GOT.func imports in two steps:

- After loading the RTS module we generate a list of (global index, function
  index) pairs of GOT.func imports. In the example above, global index is N and
  function index is the index of f0 in the defining module (the RTS).

  This is implemented in `collect_got_func_imports`.

- After merging the sections we add the functions to the table and replace
  `GOT.func` imports with globals to the functions' table indices.

  Note that we don't reuse table entries when a function is already in the
  table, to avoid breakage when [ref-types] proposal is implemented, which will
  allow mutating table entries.

  [ref-types]: https://github.com/WebAssembly/reference-types

  This is implemented in `replace_got_func_imports`.

See also the test `test/ld/fun-ptr` for a concrete exaple of GOT.func generation
and resolving.
*)

(* Linking *)

type imports = (int32 * name) list

let phrase f x = { x with it = f x.it }

let map_module f (em : extended_module) = { em with module_ = f em.module_ }
let map_name_section f (em : extended_module) = { em with name = f em.name }

(* Generic functions about import and export lists *)

let get_import is_thing j m =
  let open Int32 in
  let rec go i = function
    | [] -> assert false
    | imp::is ->
      if is_thing imp.it.idesc.it
      then
        if i = j
        then imp
        else go (add i 1l) is
      else go i is
  in go 0l m.imports

let find_imports is_thing libname m : imports =
  let name = Wasm.Utf8.decode libname in
  let rec go i acc = function
    | [] -> List.rev acc
    | imp::is ->
      if is_thing imp.it.idesc.it
      then
        if imp.it.module_name = name
        then go (i + 1) ((Int32.of_int i, imp.it.item_name) :: acc) is
        else go (i + 1) acc is
      else go i acc is
  in go 0 [] m.imports

let remove_imports is_thing resolved : module_' -> module_' = fun m ->
  let rec go i = function
    | [] -> []
    | (imp::is) ->
      if is_thing imp.it.idesc.it
      then
        if List.mem_assoc i resolved
        then go (Int32.add i 1l) is
        else imp :: go (Int32.add i 1l) is
      else imp :: go i is
  in
  { m with imports = go 0l m.imports }

let count_imports is_thing m =
  Lib.List32.length (List.filter (fun i -> is_thing i.it.idesc.it) m.imports)

let remove_export is_thing name : module_' -> module_' = fun m ->
  let to_remove e =
    not (is_thing e.it.edesc.it <> None && e.it.name = Wasm.Utf8.decode name)
  in
  { m with exports = List.filter to_remove m.exports }

module NameMap = Map.Make(struct type t = Wasm.Ast.name let compare = compare end)

type exports = int32 NameMap.t

let find_exports is_thing m : exports =
  List.fold_left (fun map exp ->
    match is_thing exp.it.edesc.it with
    | Some v -> NameMap.add exp.it.name v.it map
    | _ -> map
  ) NameMap.empty m.exports


(* Predicate to specialize these generic functions to the various entities *)

let is_fun_import = function
  | FuncImport _ -> true
  | _ -> false

let is_global_import = function
  | GlobalImport _ -> true
  | _ -> false

let is_table_import = function
  | TableImport _ -> true
  | _ -> false

let is_memory_import = function
  | MemoryImport _ -> true
  | _ -> false

let is_fun_export = function
  | FuncExport v -> Some v
  | _ -> None

let is_global_export = function
  | GlobalExport v -> Some v
  | _ -> None


let get_fun_typ i m : Wasm.Types.func_type =
  let imports_n = count_imports is_fun_import m in
  let tyvar =
    if i < imports_n
    then
      match (get_import is_fun_import i m).it.idesc.it with
      | FuncImport ty -> ty.it
      | _ -> assert false
    else
      let f = Lib.List32.nth m.funcs (Int32.sub i imports_n) in
      f.it.ftype.it
    in
  (Lib.List32.nth m.types tyvar).it

let get_global_typ i m : Wasm.Types.global_type =
  let imports_n = count_imports is_global_import m in
  if i < imports_n
  then
    match (get_import is_global_import i m).it.idesc.it with
    | GlobalImport ty -> ty
    | _ -> assert false
  else
    let f = Lib.List32.nth m.globals (Int32.sub i imports_n) in
    f.it.gtype

(* Utilities related to functions *)

let remove_fun_imports_name_section resolved : name_section -> name_section = fun ns ->
  let keep (fi, x) = not (List.mem_assoc fi resolved) in
  { ns with
    function_names = List.filter keep ns.function_names;
    locals_names = List.filter keep ns.locals_names;
  }

let prepend_to_start fi ftype (em : extended_module)  =
  let imports_n = count_imports is_fun_import em.module_ in
  let wrap_fi = Int32.add imports_n (Lib.List32.length em.module_.funcs) in

  let wrap_fun = {
    ftype = ftype @@ no_region;
    locals = [];
    body =
      [ Call (fi @@ no_region) @@ no_region ] @
      (match em.module_.start with
        | Some start_fi -> [ Call start_fi @@ no_region ]
        | None -> [])
    } @@ no_region in

  { em with
    module_ =
      { em.module_ with
        funcs = em.module_.funcs @ [ wrap_fun ];
        start = Some (wrap_fi @@ no_region)
      };
    name =
      { em.name with
        function_names = em.name.function_names @ [ wrap_fi, "link_start" ]
      }
  }

let _remove_non_canister_exports (em : extended_module) : extended_module =
  let is_canister_export (exp : export) = Lib.String.chop_prefix "canister_" (Wasm.Utf8.encode exp.it.name) <> None in
  map_module (fun m -> { m with exports = List.filter is_canister_export m.exports }) em

let remove_non_ic_exports (em : extended_module) : extended_module =
  (* We assume that every exported function that does not have an entry in the
   custom types section was only exported for linking, and should not be
   exported in the final module *)
  let is_ic_export (exp : export) =
    Lib.String.chop_prefix "canister_" (Wasm.Utf8.encode exp.it.name) <> None ||
    "_start" = Wasm.Utf8.encode exp.it.name
  in

  let keep_export exp =
    is_ic_export exp ||
    match exp.it.edesc.it with
      | FuncExport _
      | GlobalExport _ -> false
      | MemoryExport _
      | TableExport _ -> true in

  map_module (fun m -> { m with exports = List.filter keep_export m.exports }) em

(* Generic linking logic *)

exception LinkError of string

type renumbering = int32 -> int32

let resolve imports exports : (int32 * int32) list =
  List.flatten (List.map (fun (fi, name) ->
    match NameMap.find_opt name exports with
    | Some fi' -> [ (fi, fi') ]
    | None -> []
    ) imports)

let calculate_renaming n_imports1 n_things1 n_imports2 resolved12 resolved21 : (renumbering * renumbering) =
  let open Int32 in

  let n_imports1' = sub n_imports1 (Lib.List32.length resolved12) in
  let n_imports2' = sub n_imports2 (Lib.List32.length resolved21) in

  let rec fun1 i =
    let rec go skipped = function
      | (imp, exp)::is ->
        if i < imp then sub i skipped
        else if i = imp then fun2 exp
        else go (add skipped 1l) is
      | [] ->
        if i < n_imports1
        then sub i skipped
        else sub (add i n_imports2') skipped
    in go 0l resolved12
  and fun2 i =
    let rec go skipped = function
      | (imp, exp)::is ->
        if i < imp then sub (add i n_imports1') skipped
        else if i = imp then fun1 exp
        else go (add skipped 1l) is
      | [] ->
        if i < n_imports2
        then sub (add i n_imports1') skipped
        else sub (add (add i n_imports1') n_things1) skipped
    in go 0l resolved21
  in
  (fun1, fun2)


(* AST traversals *)

let rename_funcs rn : module_' -> module_' = fun m ->
  let var' = rn in
  let var = phrase var' in

  let rec instr' = function
    | Call v -> Call (var v)
    | Block (ty, is) -> Block (ty, instrs is)
    | Loop (ty, is) -> Loop (ty, instrs is)
    | If (ty, is1, is2) -> If (ty, instrs is1, instrs is2)
    | i -> i
  and instr i = phrase instr' i
  and instrs is = List.map instr is in

  let func' f = { f with body = instrs f.body } in
  let func = phrase func' in
  let funcs = List.map func in

  let edesc' = function
    | FuncExport v -> FuncExport (var v)
    | e -> e in
  let edesc = phrase edesc' in
  let export' e = { e with edesc = edesc e.edesc } in
  let export = phrase export' in
  let exports = List.map export in

  let segment' f s = { s with init  = f s.init } in
  let segment f = phrase (segment' f) in

  { m with
    funcs = funcs m.funcs;
    exports = exports m.exports;
    start = Option.map var m.start;
    elems = List.map (segment (List.map var)) m.elems;
  }

let rename_globals rn : module_' -> module_' = fun m ->
  let var' = rn in
  let var = phrase var' in

  let rec instr' = function
    | Block (ty, is) -> Block (ty, instrs is)
    | Loop (ty, is) -> Loop (ty, instrs is)
    | If (ty, is1, is2) -> If (ty, instrs is1, instrs is2)
    | GlobalGet v -> GlobalGet (var v)
    | GlobalSet v -> GlobalSet (var v)
    | i -> i
  and instr i = phrase instr' i
  and instrs is = List.map instr is in

  let func' f = { f with body = instrs f.body } in
  let func = phrase func' in
  let funcs = List.map func in

  let const = phrase instrs in

  let global' g = { g with value = const g.value } in
  let global = phrase global' in
  let globals = List.map global in

  let table_segment' (s : var list segment') = { s with offset = const s.offset; } in
  let table_segment = phrase (table_segment') in
  let table_segments = List.map table_segment in

  let memory_segment' (s : string segment') = { s with offset = const s.offset; } in
  let memory_segment = phrase (memory_segment') in
  let memory_segments = List.map memory_segment in


  { m with
    funcs = funcs m.funcs;
    globals = globals m.globals;
    elems = table_segments m.elems;
    data = memory_segments m.data;
  }

let set_global global value = fun m ->
  let rec go i = function
    | [] -> assert false
    | g::gs when i = Int32.to_int global ->
      let open Wasm.Types in
      assert (g.it.gtype = GlobalType (I32Type, Immutable));
      let g = phrase (fun g' ->
        { g' with value = [Const (Wasm.Values.I32 value @@ g.at) @@ g.at] @@ g.at }
      ) g in
      g :: gs
    | g::gs -> g :: go (i+1) gs
  in
  { m with globals = go 0 m.globals }

let fill_global (global : int32) (value : int32) : module_' -> module_' = fun m ->
  let rec instr' = function
    | Block (ty, is) -> Block (ty, instrs is)
    | Loop (ty, is) -> Loop (ty, instrs is)
    | If (ty, is1, is2) -> If (ty, instrs is1, instrs is2)
    | GlobalGet v when v.it = global -> Const (Wasm.Values.I32 value @@ v.at)
    | GlobalSet v when v.it = global -> assert false
    | i -> i
  and instr i = phrase instr' i
  and instrs is = List.map instr is in

  let func' f = { f with body = instrs f.body } in
  let func = phrase func' in
  let funcs = List.map func in

  let const = phrase instrs in

  let global' g = { g with value = const g.value } in
  let global = phrase global' in
  let globals = List.map global in

  let table_segment' (s : var list segment') = { s with offset = const s.offset; } in
  let table_segment = phrase (table_segment') in
  let table_segments = List.map table_segment in

  let memory_segment' (s : string segment') = { s with offset = const s.offset; } in
  let memory_segment = phrase (memory_segment') in
  let memory_segments = List.map memory_segment in


  { m with
    funcs = funcs m.funcs;
    globals = globals m.globals;
    elems = table_segments m.elems;
    data = memory_segments m.data;
  }

let rename_funcs_name_section rn (ns : name_section) =
  { ns with
    function_names = List.map (fun (fi, name) -> (rn fi, name)) ns.function_names;
    locals_names = List.map (fun (fi, locals) -> (rn fi, locals)) ns.locals_names;
  }

let rename_funcs_extended rn (em : extended_module) =
  { em with
    module_ = rename_funcs rn em.module_;
    name = rename_funcs_name_section rn em.name;
  }

let rename_globals_extended rn (em : extended_module) =
  { em with
    module_ = rename_globals rn em.module_;
  }

let rename_types rn m =
  let ty_var = phrase rn in

  let block_type = function
    | VarBlockType tv -> VarBlockType (ty_var tv)
    | ValBlockType vto -> ValBlockType vto in

  let rec instr' = function
    | CallIndirect tv -> CallIndirect (ty_var tv)
    | Block (bty, is) -> Block (block_type bty, instrs is)
    | Loop (bty, is) -> Loop (block_type bty, instrs is)
    | If (bty, is1, is2) -> If (block_type bty, instrs is1, instrs is2)
    | i -> i
  and instr i = phrase instr' i
  and instrs is = List.map instr is in

  let func' f = { f with ftype = ty_var f.ftype; body = instrs f.body } in
  let func = phrase func' in
  let funcs = List.map func in

  let idesc' = function
    | FuncImport tv -> FuncImport (ty_var tv)
    | id -> id in
  let idesc = phrase idesc' in
  let import' i = { i with idesc = idesc i.idesc } in
  let import = phrase import' in
  let imports = List.map import in

  { m with
    funcs = funcs m.funcs;
    imports = imports m.imports;
  }

(* Setting and getting top-level module data *)

let read_global gi (m : module_') : int32 =
  let n_impo = count_imports is_global_import m in
  let g = List.nth m.globals (Int32.(to_int (sub gi n_impo))) in
  let open Wasm.Types in
  assert (g.it.gtype = GlobalType (I32Type, Immutable));
  match g.it.value.it with
  | [{ it = Const {it = Wasm.Values.I32 i;_}; _}] -> i
  | _ -> assert false

let read_table_size (m : module_') : int32 =
  (* Assumes there is one table *)
  let open Wasm.Types in
  match m.tables with
  | [t] ->
    let TableType ({min;max}, _) = t.it.ttype in
    if Some min <> max
    then raise (LinkError "Expect fixed sized table in first module")
    else min
  | _ -> raise (LinkError "Expect one table in first module")

let set_memory_size new_size_bytes : module_' -> module_' = fun m ->
  let open Wasm.Types in
  let page_size = Int32.of_int (64*1024) in
  let new_size_pages = Int32.(add (div new_size_bytes page_size) 1l) in
  match m.memories with
  | [t] ->
    { m with
      memories = [ phrase (fun m ->
        { mtype = MemoryType ({min = new_size_pages; max = None}) }
      ) t ]
    }
  | _ -> raise (LinkError "Expect one memory in first module")

let set_table_size new_size : module_' -> module_' = fun m ->
  let open Wasm.Types in
  match m.tables with
  | [t] ->
    { m with
      tables = [ phrase (fun t ->
        let TableType (_, ty) = t.ttype in
        { ttype = TableType ({min = new_size; max = Some new_size}, ty) }
      ) t ]
    }
  | _ -> raise (LinkError "Expect one table in first module")

let fill_memory_base_import new_base : module_' -> module_' = fun m ->
  (* We need to find the right import,
     replace all uses of get_global of that import with the constant,
     and finally rename all globals
  *)
  let base_global =
    let rec go i = function
      | [] -> assert false
      | imp::is -> match imp.it.idesc.it with
        | GlobalImport _ty
          when imp.it.module_name = Wasm.Utf8.decode "env" &&
               imp.it.item_name = Wasm.Utf8.decode "__memory_base" ->
          Int32.of_int i
        | GlobalImport _ ->
          go (i + 1) is
        | _ ->
          go i is
    in go 0 m.imports in

    m |> fill_global base_global new_base
      |> remove_imports is_global_import [(base_global, base_global)]
      |> rename_globals Int32.(fun i ->
          if i < base_global then i
          else if i = base_global then assert false
          else sub i 1l
        )

let fill_table_base_import new_base : module_' -> module_' = fun m ->
  (* We need to find the right import,
     replace all uses of get_global of that import with the constant,
     and finally rename all globals
  *)
  let base_global =
    let rec go i = function
      | [] -> assert false
      | imp::is -> match imp.it.idesc.it with
        | GlobalImport _ty
          when imp.it.module_name = Wasm.Utf8.decode "env" &&
               imp.it.item_name = Wasm.Utf8.decode "__table_base" ->
          Int32.of_int i
        | GlobalImport _ ->
          go (i + 1) is
        | _ ->
          go i is
    in go 0 m.imports in

    m |> fill_global base_global new_base
      |> remove_imports is_global_import [(base_global, base_global)]
      |> rename_globals Int32.(fun i ->
          if i < base_global then i
          else if i = base_global then assert false
          else sub i 1l
        )


(* Concatenation of modules *)

let join_modules (em1 : extended_module) (m2 : module_') (ns2 : name_section) : extended_module =
  assert (m2.start = None);
  let m1 = em1.module_ in
  { em1 with
    module_ = {
      types = m1.types @ m2.types;
      globals = m1.globals @ m2.globals;
      tables = m1.tables @ m2.tables;
      memories = m1.memories @ m2.memories;
      funcs = m1.funcs @ m2.funcs;
      start = m1.start;
      elems = m1.elems @ m2.elems;
      data = m1.data @ m2.data;
      imports = m1.imports @ m2.imports;
      exports = m1.exports @ m2.exports;
    };
    name = {
      em1.name with
      function_names = em1.name.function_names @ ns2.function_names;
      locals_names = em1.name.locals_names @ ns2.locals_names;
      };
    motoko = em1.motoko;
  }

(* The main linking function *)

let check_typ is_thing get_typ string_of m1 m2 (i1, i2) =
  let t1 = get_typ i1 m1 in
  let t2 = get_typ i2 m2 in
  let imp = get_import is_thing i1 m1 in
  if t1 <> t2 then
    let msg = Printf.sprintf
      "Type mismatch when linking %s.%s:\nimport type: %s\nexport type: %s"
      (string_of_name imp.it.module_name)
      (string_of_name imp.it.item_name)
      (string_of t1)
      (string_of t2)
    in
    raise (LinkError msg)

let check_fun_typ =
  check_typ is_fun_import get_fun_typ Wasm.Types.string_of_func_type
let check_global_typ =
  check_typ is_global_import get_global_typ Wasm.Types.string_of_global_type


let align p n =
  let open Int32 in
  let p = to_int p in
  shift_left (shift_right_logical (add n (sub (shift_left 1l p) 1l)) p) p

let find_fun_export (name : name) (exports : export list) : var option =
  List.find_map (fun (export : export) ->
    if export.it.name = name then
      match export.it.edesc.it with
      | FuncExport var -> Some var
      | _ -> raise (LinkError (Format.sprintf "Export %s is not a function" (Wasm.Utf8.encode name)))
    else
      None
  ) exports

let remove_got_func_imports (imports : import list) : import list =
  let got_func_str = Wasm.Utf8.decode "GOT.func" in
  List.filter (fun import -> import.it.module_name <> got_func_str) imports

(* Merge global list of a module with a sorted (on global index) list of (global
   index, global) pairs, overriding globals at those indices, and appending
   left-overs at the end. *)
let add_globals (globals0 : global list) (insert0 : (int32 * global') list) : global list =
  let rec go (current_idx : int32) globals insert =
    match insert with
    | [] -> globals
    | (insert_idx, global) :: rest ->
      if current_idx = insert_idx then
        (global @@ no_region) :: go (Int32.add current_idx 1l) globals rest
      else
        match globals with
        | [] -> List.map (fun (_, global) -> global @@ no_region) insert
        | global :: globals -> global :: go (Int32.add current_idx 1l) globals rest
  in
  go 0l globals0 insert0

let mk_i32_const (i : int32) =
  Const (Wasm.Values.I32 i @@ no_region) @@ no_region

let mk_i32_global (i : int32) =
  { gtype = Wasm.Types.GlobalType (Wasm.Types.I32Type, Wasm.Types.Immutable);
    value = [mk_i32_const i] @@ no_region }

(* Generate (global index, function index) pairs for GOT.func imports of a
   module. Uses import and export lists of the module so those should be valid. *)
let collect_got_func_imports (m : module_') : (int32 * int32) list =
  let got_func_name = Wasm.Utf8.decode "GOT.func" in

  let get_got_func_import (global_idx, imports) import : (int32 * (int32 * int32) list) =
    if import.it.module_name = got_func_name then
      (* Found a GOT.func import, find the exported function for it *)
      let name = import.it.item_name in
      let fun_idx =
        match find_fun_export name m.exports with
        | None -> raise (LinkError (Format.sprintf "Can't find export for GOT.func import %s" (Wasm.Utf8.encode name)))
        | Some export_idx -> export_idx.it
      in
      let global_idx =
        if is_global_import import.it.idesc.it then
          global_idx
        else
          raise (LinkError "GOT.func import is not global")
      in
      ( Int32.add global_idx (Int32.of_int 1), (global_idx, fun_idx) :: imports )
    else
      let global_idx =
        if is_global_import import.it.idesc.it then
          Int32.add global_idx (Int32.of_int 1)
        else
          global_idx
      in
      ( global_idx, imports )
  in

  (* (global index, function index) list *)
  let (_, got_func_imports) =
    List.fold_left get_got_func_import (0l, []) m.imports
  in

  got_func_imports

(* Add functions imported from GOT.func to the table, replace GOT.func imports
   with globals to the table indices.

   `tbe_size` is the size of the table in the merged module before adding
   GOT.func functions. *)
let replace_got_func_imports (tbl_size : int32) (imports : (int32 * int32) list) (m : module_') : module_' =
  (* null check to avoid adding empty elem section *)
  if imports = [] then
    m
  else
    let imports =
      List.sort (fun (gbl_idx_1, _) (gbl_idx_2, _) -> compare gbl_idx_1 gbl_idx_2) imports
    in

    let elems : var list =
      List.map (fun (_, fun_idx) -> fun_idx @@ no_region) imports
    in

    let elem_section =
      { index = 0l @@ no_region; offset = [ mk_i32_const tbl_size ] @@ no_region; init = elems }
    in

    let globals =
      List.mapi (fun idx (global_idx, _) -> (global_idx, mk_i32_global (Int32.add tbl_size (Int32.of_int idx)))) imports
    in

    { m with
      elems = List.append m.elems [elem_section @@ no_region];
      imports = remove_got_func_imports m.imports;
      globals = add_globals m.globals globals
    }

(* The first argument specifies the global of the first module indicating the
start of free memory *)
let link (em1 : extended_module) libname (em2 : extended_module) =

  let global_exports1 = find_exports is_global_export em1.module_ in

  let heap_global =
    match NameMap.find_opt (Wasm.Utf8.decode "__heap_base") global_exports1 with
    | None -> raise (LinkError "First module does not export __heap_base")
    | Some gi -> gi in

  let dylink = match em2.dylink with
    | Some dylink -> dylink
    | None -> raise (LinkError "Second module does not have a dylink section") in

  (* Beginning of unused space *)
  let old_heap_start = read_global heap_global em1.module_ in
  let lib_heap_start = align dylink.memory_alignment old_heap_start in
  let new_heap_start = align 4l (Int32.add lib_heap_start dylink.memory_size) in

  let old_table_size = read_table_size em1.module_ in
  let lib_table_start = align dylink.table_alignment old_table_size in

  (* Fill in memory and table base pointers *)
  let dm2 = em2.module_
    |> fill_memory_base_import lib_heap_start
    |> fill_table_base_import lib_table_start in

  let got_func_imports = collect_got_func_imports dm2 in

  (* Link functions *)
  let fun_required1 = find_imports is_fun_import libname em1.module_ in
  let fun_required2 = find_imports is_fun_import "env" dm2 in
  let fun_exports1 = find_exports is_fun_export em1.module_ in
  let fun_exports2 = find_exports is_fun_export dm2 in
  (* Resolve imports, to produce a renumbering function: *)
  let fun_resolved12 = resolve fun_required1 fun_exports2 in
  let fun_resolved21 = resolve fun_required2 fun_exports1 in
  let (funs1, funs2) =
    calculate_renaming
      (count_imports is_fun_import em1.module_)
      (Lib.List32.length em1.module_.funcs)
      (count_imports is_fun_import dm2)
      fun_resolved12
      fun_resolved21 in

  List.iter (check_fun_typ em1.module_ dm2) fun_resolved12;
  List.iter (check_fun_typ dm2 em1.module_) fun_resolved21;

  (* Link globals *)
  let global_required1 = find_imports is_global_import libname em1.module_ in
  let global_required2 = find_imports is_global_import "env" dm2 in
  let global_exports2 = find_exports is_global_export dm2 in
  (* Resolve imports, to produce a renumbering *)
  let global_resolved12 = resolve global_required1 global_exports2 in
  let global_resolved21 = resolve global_required2 global_exports1 in
  let (globals1, globals2) =
    calculate_renaming
      (count_imports is_global_import em1.module_)
      (Lib.List32.length em1.module_.globals)
      (count_imports is_global_import dm2)
      global_resolved12
      global_resolved21 in
  assert (global_required1 = []); (* so far, we do not import globals *)

  List.iter (check_global_typ em1.module_ dm2) global_resolved12;
  List.iter (check_global_typ dm2 em1.module_) global_resolved21;

  (* Rename types in both modules to eliminate duplicate types. *)

  (* Maps function types to their indices in the new module we're creating *)
  let type_indices : (Wasm.Types.func_type, int32) Hashtbl.t = Hashtbl.create 100 in

  (* Get index of a function type. Creates a new one if we haven't added this
     type yet. *)
  let add_or_get_ty (ty : Wasm.Types.func_type) =
    match Hashtbl.find_opt type_indices ty with
    | None ->
      let idx = Int32.of_int (Hashtbl.length type_indices) in
      Hashtbl.add type_indices ty idx;
      idx
    | Some idx ->
      idx
  in

  (* Rename a type in a module. First argument is the list of types in the module. *)
  let ty_renamer (tys : Wasm.Types.func_type phrase list) (t : int32) : int32 =
    let fun_ty = List.nth tys (Int32.to_int t) in
    add_or_get_ty fun_ty.it
  in

  (* Rename types in first module *)
  let em1_tys =
    map_module (fun m -> { (rename_types (ty_renamer m.types) m) with types = [] }) em1
  in

  (* Rename types in second module *)
  let dm2 =
    { (rename_types (ty_renamer dm2.types) dm2) with types = [] }
  in

  (* Generate type section for the final module *)
  let type_indices_sorted : type_ list =
    Hashtbl.to_seq type_indices |>
    List.of_seq |>
    List.sort (fun (_, idx1) (_, idx2) -> compare idx1 idx2) |>
    List.map (fun (ty, _) -> ty @@ no_region)
  in

  (* Inject call to "__wasm_call_ctors" *)
  let add_call_ctors =
    match NameMap.find_opt (Wasm.Utf8.decode "__wasm_call_ctors") fun_exports2 with
    | None -> fun em -> em
    | Some fi -> prepend_to_start (funs2 fi) (add_or_get_ty (Wasm.Types.FuncType ([], [])))
  in

  assert (dm2.globals = []);

  let new_table_size =
    Int32.add (Int32.add lib_table_start dylink.table_size) (Int32.of_int (List.length got_func_imports))
  in

  let merged = join_modules
    ( em1_tys
    |> map_module (fun m -> { m with types = type_indices_sorted })
    |> map_module (remove_imports is_fun_import fun_resolved12)
    |> map_name_section (remove_fun_imports_name_section fun_resolved12)
    |> map_module (remove_imports is_global_import global_resolved12)
    |> rename_funcs_extended funs1
    |> rename_globals_extended globals1
    |> map_module (set_global heap_global new_heap_start)
    |> map_module (set_memory_size new_heap_start)
    |> map_module (set_table_size new_table_size)
    )
    ( dm2
    |> remove_imports is_fun_import fun_resolved21
    |> remove_imports is_global_import global_resolved21
    |> remove_imports is_memory_import [0l, 0l]
    |> remove_imports is_table_import [0l, 0l]
    |> rename_funcs funs2
    |> rename_globals globals2
    |> remove_export is_fun_export "__wasm_call_ctors"
    )
    ( em2.name
    |> remove_fun_imports_name_section fun_resolved21
    |> rename_funcs_name_section funs2
    )
  |> add_call_ctors
  |> remove_non_ic_exports (* only sane if no additional files get linked in *)
  in

  (* Rename global and function indices in GOT.func stuff *)
  let got_func_imports =
    List.map (fun (global_idx, func_idx) -> (globals2 global_idx, funs2 func_idx)) got_func_imports
  in

  (* Replace GOT.func imports with globals to function table indices *)
  let final =
    replace_got_func_imports (Int32.add lib_table_start dylink.table_size) got_func_imports merged.module_
  in

  { merged with module_ = final }
