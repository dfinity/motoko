open Wasm.Ast

type name_section = {
  module_ : string;
  function_names : (int32 * string) list;
  locals_names : (int32 * (int32 * string) list) list;
}

type dylink = {
  memory_size : int32;
  memory_alignment : int32;
  table_size : int32;
  table_alignment : int32;
  needed_dynlibs : string list;
}

type dylink_module = {
  module_ : module_;
  dylink : dylink;
  name : name_section;
}

