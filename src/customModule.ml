(* Extend the idea of a module as defined in Wasm.Syntax
   with custom sections that we are interested in
*)

open Wasm.Source
open Wasm.Ast
open Dylib

type extended_module = {
  (* The non-custom sections *)
  module_ : module_;
  (* function index, and number of arguments (simplified type system) *)
  types : (int32 * CustomSections.type_ list) list;
  (* index of persisted global, and its type *)
  persist : (int32 * CustomSections.type_) list;
  (* name section *)
  name : name_section;
  }

let is_fun_import (i : import) = match i.it.idesc.it with
  | FuncImport _ -> true
  | _            -> false

let encode (em : extended_module) =
  let (map, wasm) = EncodeMap.encode em.module_ in
  let custom_sections = CustomSections.encode
    (Int32.of_int (List.length (List.filter is_fun_import em.module_.it.imports)))
    em.types
    em.persist
    em.name
  in (map, wasm ^ custom_sections)
