(* Extend the idea of a module as defined in Wasm.Syntax
   with custom sections that we are interested in
*)

open Wasm.Source
open Wasm.Ast

type extended_module = {
  (* The non-custom sections *)
  module_ : module_;
  (* function index, and number of arguments (simplified type system) *)
  types : (int32 * int32) list;
  (* index of persisted global, and its type *)
  persist : (int32 * CustomSections.persistSort) list;
  (* Function names *)
  function_names : (int32 * string) list
  }

let encode m =
  let (map, wasm) = EncodeMap.encode m.module_ in
  let custom_sections = CustomSections.encode
    (Int32.of_int (List.length (m.module_.it.imports)))
    m.types
    m.persist
    m.function_names
  in (map, wasm ^ custom_sections)
