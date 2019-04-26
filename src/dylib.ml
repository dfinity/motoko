open Wasm.Ast

type dylink = {
  memory_size : int32;
  memory_alignment : int32;
  table_size : int32;
  table_alignment : int32;
  needed_dynlibs : string list;
}

type dylink_module = {
  module_ : module_;
  dylink :  dylink
}

