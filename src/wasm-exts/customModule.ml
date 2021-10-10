(* Extend the idea of a module as defined in Wasm.Syntax
   with custom sections that we are interested in
*)

open Ast

type name_section = {
  module_ : string option;
  function_names : (int32 * string) list;
  locals_names : (int32 * (int32 * string) list) list;
  label_names : (int32 * (int32 * string) list) list;
  type_names : (int32 * string) list;
  table_names : (int32 * string) list;
  memory_names : (int32 * string) list;
  global_names : (int32 * string) list;
  elem_segment_names : (int32 * string) list;
  data_segment_names : (int32 * string) list;
}

let empty_name_section : name_section = {
  module_ = None;
  function_names = [];
  locals_names = [];
  label_names = [];
  type_names = [];
  table_names = [];
  memory_names = [];
  global_names = [];
  elem_segment_names = [];
  data_segment_names = [];
}

type dylink_section = {
  memory_size : int32;
  memory_alignment : int32;
  table_size : int32;
  table_alignment : int32;
  needed_dynlibs : string list;
}

type motoko_section = {
  labels : string list;
  sig_ : string;
}

let empty_motoko_section = {
    labels = [];
    sig_ = "";
  }

let empty_candid_section = ""

type extended_module = {
  (* The non-custom sections *)
  module_ : module_';
  (* name section *)
  name : name_section;
  (* dylib section *)
  dylink : dylink_section option;
  (* motoko section *)
  motoko : motoko_section;
  (* candid section *)
  candid : string (* option?*);
  (* source map section *)
  source_mapping_url : string option;
}
