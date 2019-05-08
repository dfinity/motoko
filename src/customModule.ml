(* Extend the idea of a module as defined in Wasm.Syntax
   with custom sections that we are interested in
*)

open Wasm.Source
open Wasm.Ast

type extended_module = {
  (* The non-custom sections *)
  module_ : module_;
  (* function index, and number of arguments (simplified type system) *)
  types : (int32 * CustomSections.type_ list) list;
  (* index of persisted global, and its type *)
  persist : (int32 * CustomSections.type_) list;
  (* Module name *)
  module_name : string;
  (* Function names *)
  function_names : (int32 * string) list;
  (* Names of locals *)
  locals_names : (int32 * (int32 * string) list) list;
  (* Hashes of Labels (fields and variants) *)
  labels : (int32 * string) list;
  }

let is_fun_import (i : import) = match i.it.idesc.it with
  | FuncImport _ -> true
  | _            -> false

let encode m =
  let (map, wasm) = EncodeMap.encode m.module_ in
  let custom_sections = CustomSections.encode
    (Int32.of_int (List.length (List.filter is_fun_import m.module_.it.imports)))
    m.types
    m.persist
    m.module_name
    m.function_names
    m.locals_names
    m.labels
  in (map, wasm ^ custom_sections)
