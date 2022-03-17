(* common flags for the moc compiler *)

module M = Map.Make(String)

type compile_mode = WasmMode | ICMode | RefMode | WASIMode

type gc_strategy = MarkCompact | Copying

let trace = ref false
let verbose = ref false
let print_warnings = ref true
let warnings_are_errors = ref false
let print_depth = ref 2
let release_mode = ref false
let compile_mode = ref ICMode
let debug_info = ref false
let multi_value = ref false
let await_lowering = ref true
let async_lowering = ref true
let dump_parse = ref false
let dump_tc = ref false
let dump_lowering = ref false
let check_ir = ref true
let package_urls : string M.t ref = ref M.empty
let actor_aliases : string M.t ref = ref M.empty
let actor_idl_path : string option ref = ref None
let max_stable_pages_default = 65536
let max_stable_pages : int ref = ref max_stable_pages_default
let pre_ref : string option ref = ref None
let post_ref : string option ref = ref None
let profile = ref false
let profile_verbose = ref false
let profile_file = ref "profiling-counters.csv"
let profile_line_prefix = ref ""
let profile_field_names : string list ref = ref []
let public_metadata_names : string list ref = ref []
let suppress_metadata_names : string list ref = ref []
let compiled = ref false
let error_detail = ref 2
let sanity = ref false
let gc_strategy = ref Copying
let force_gc = ref false
