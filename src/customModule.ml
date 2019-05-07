(* Extend the idea of a module as defined in Wasm.Syntax
   with custom sections that we are interested in
*)

open Wasm.Ast

type type_ = I32 | DataBuf | ElemBuf | ActorRef | FuncRef

type name_section = {
  module_ : string option;
  function_names : (int32 * string) list;
  locals_names : (int32 * (int32 * string) list) list;
}

type dylink_section = {
  memory_size : int32;
  memory_alignment : int32;
  table_size : int32;
  table_alignment : int32;
  needed_dynlibs : string list;
}

type extended_module = {
  (* The non-custom sections *)
  module_ : module_;
  (* name section *)
  name : name_section;
  (* dylib section *)
  dylink : dylink_section option;
  (* DFINITY Types: *)
  (* function index, and number of arguments (simplified type system) *)
  types : (int32 * type_ list) list;
  (* index of persisted global, and its type *)
  persist : (int32 * type_) list;
  }
