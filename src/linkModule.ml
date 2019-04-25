(*
The type for a dynamic library: A normal WebAssembly module
plus the dylink section.
*)

open Wasm.Ast
open Wasm.Source
open CustomModule

type dylink = {
  memorysize : int32;
  memoryalignment : int32;
  tablesize : int32;
  tablealignment : int32;
  needed_dynlibs : string list;
}

type dylink_module = {
  module_ : module_;
  dylink :  dylink
}

let decode name bs =
  let module_ = Wasm.Decode.decode name bs in
  { module_;
    dylink = {
      memorysize = 0l;
      memoryalignment = 0l;
      tablesize = 0l;
      tablealignment = 0l;
      needed_dynlibs = [];
    }
  }

type imports = (int32 * name) list

let phrase f x = { x with it = f x.it }

let in_extended f (em : extended_module) = { em with module_ = f em.module_ }

let find_imports libname m : imports =
  let name = Wasm.Utf8.decode libname in
  let rec go i acc = function
    | [] -> List.rev acc
    | imp::is -> match imp.it.idesc.it with
      | FuncImport _ty when imp.it.module_name = name ->
        go (i + 1) ((Int32.of_int i, imp.it.item_name) :: acc) is
      | FuncImport _ ->
        go (i + 1) acc is
      | _ ->
        go i acc is
  in go 0 [] m.it.imports

let remove_imports libname : module_ -> module_ =
  phrase (fun m ->
    let name = Wasm.Utf8.decode libname in
    let go imp = match imp.it.idesc.it with
        | FuncImport _ty when imp.it.module_name = name -> false
        | _ -> true in
    { m with imports = List.filter go m.imports }
  )

let remove_table_import : module_ -> module_ =
  phrase (fun m ->
    let go imp = match imp.it.idesc.it with
        | TableImport _ -> false
        | _ -> true in
    { m with imports = List.filter go m.imports }
  )

let remove_memory_import : module_ -> module_ =
  phrase (fun m ->
    let go imp = match imp.it.idesc.it with
        | MemoryImport _ -> false
        | _ -> true in
    { m with imports = List.filter go m.imports }
  )

let remove_function_exports : module_ -> module_ =
  phrase (fun m ->
    let go exp = match exp.it.edesc.it with
        | FuncExport _var -> false
        | _ -> true in
    { m with exports = List.filter go m.exports }
  )

let remove_base_imports : module_ -> module_ =
  phrase (fun m ->
    let go imp = match imp.it.idesc.it with
        | GlobalImport _
          when imp.it.module_name = Wasm.Utf8.decode "env" &&
               (imp.it.item_name = Wasm.Utf8.decode "__memory_base" ||
                imp.it.item_name = Wasm.Utf8.decode "__table_base")
          -> false
        | _ -> true in
    { m with imports = List.filter go m.imports }
  )

module NameMap = Map.Make(struct type t = Wasm.Ast.name let compare = compare end)
type exports = int32 NameMap.t
let find_exports m : exports =
  List.fold_left
    (fun map exp ->
      match exp.it.edesc.it with
      | FuncExport fi -> NameMap.add exp.it.name fi.it map
      | _ -> map
    )
    NameMap.empty m.it.exports

let count_functions m : int =
  let rec go i = function
    | [] -> i
    | imp::is -> match imp.it.idesc.it with
      | FuncImport _ -> go (i + 1) is
      | _ -> go i is
  in go 0 m.it.imports + List.length (m.it.funcs)

exception LinkError of string

type func_renumbering = int32 -> int32
let resolve offset2 imports exports : func_renumbering =
  let resolved = List.map (fun (fi, name) ->
    match NameMap.find_opt name exports with
    | Some fi' -> (fi, Int32.add offset2 fi')
    | None -> raise (LinkError (Printf.sprintf "Unresolvd symbol %s" (Wasm.Utf8.encode name)))
    ) imports in

  (* Obviously algorithmically not great *)
  fun i ->
    let rec go offset = function
      | [] -> Int32.sub i offset
      | (imp, exp)::is ->
        if i < imp then Int32.sub i offset
        else if i = imp then exp
        else go (Int32.add offset 1l) is
    in go 0l resolved

let join_modules (em1 : extended_module) (m2 : module_) : extended_module =
  assert (m2.it.start = None);
  let m1 = em1.module_ in
  { em1 with module_ =
    { it =
      { types = m1.it.types @ m2.it.types
      ; globals = m1.it.globals @ m2.it.globals
      ; tables = m1.it.tables @ m2.it.tables
      ; memories = m1.it.memories @ m2.it.memories
      ; funcs = m1.it.funcs @ m2.it.funcs
      ; start = m1.it.start
      ; elems = m1.it.elems @ m2.it.elems
      ; data = m1.it.data @ m2.it.data
      ; imports = m1.it.imports @ m2.it.imports
      ; exports = m1.it.exports @ m2.it.exports
      }
    ; at = m1.at
    }
  }

let rename_funcs rn : module_ -> module_ =
  phrase (fun m ->
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

    let segment' f s = { s with init  = f s.init } in
    let segment f = phrase (segment' f) in

    { m with
      funcs = funcs m.funcs;
      start = Lib.Option.map var m.start;
      elems = List.map (segment (List.map var)) m.elems;
    }
  )

let rename_funcs_extended rn (em : extended_module) =
    { em with
      module_ = rename_funcs rn em.module_;
      types = List.map (fun (fi, ty) -> (rn fi, ty)) em.types;
      function_names = List.map (fun (fi, name) -> (rn fi, name)) em.function_names;
      locals_names = List.map (fun (fi, locals) -> (rn fi, locals)) em.locals_names;
    }

let rename_types rn m =
  let phrase f x = { x with it = f x.it } in
  let ty_var' = rn in
  let ty_var = phrase ty_var' in

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
    it = { m.it with
      funcs = funcs m.it.funcs;
      imports = imports m.it.imports;
    }
  }

(* The first argument specifies the global of the first module indicating the
start of free memory *)
let link heap_ptr (em : extended_module) libname (dm : dylink_module) =
  (* First we look into m to see which imports we have *)
  let required = find_imports libname em.module_ in
  (* And also how many functions in total *)
  let funcs1_before = count_functions em.module_ in
  let fun_offset2 = Int32.of_int (funcs1_before - List.length required) in
  let ty_offset2 = Int32.of_int (List.length (em.module_.it.types)) in
  (* Find exported functions from the second module *)
  let exports = find_exports dm.module_ in
  (* Resolve imports, to produce a renumbering function: *)
  let funs1 = resolve fun_offset2 required exports in
  let funs2 = fun i -> Int32.add fun_offset2 i in

  assert (dm.module_.it.globals = []);

  join_modules
    ( em
    |> in_extended (remove_imports libname)
    |> rename_funcs_extended funs1
    )
    ( dm.module_
    |> remove_function_exports
    |> remove_table_import
    |> remove_memory_import
    |> rename_funcs funs2
    |> rename_types (fun t -> Int32.(add t ty_offset2))
    |> remove_base_imports
    )
