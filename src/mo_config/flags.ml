(* common flags for the moc compiler *)

module M = Map.Make(String)

type compile_mode = WasmMode | ICMode | RefMode | WASIMode

type gc_strategy = Default | MarkCompact | Copying | Generational | Incremental

type instruction_limits = {
  upgrade: int;
  update_call: int;
}

let ai_errors = ref false
let trace = ref false
let verbose = ref false
let print_warnings = ref true
let warnings_are_errors = ref false
let print_source_on_error = ref false
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
let measure_rts_stack = ref false
let pre_ref : string option ref = ref None
let post_ref : string option ref = ref None
let profile = ref false
let profile_verbose = ref false
let profile_file = ref "profiling-counters.csv"
let profile_line_prefix = ref ""
let profile_field_names : string list ref = ref []
let public_metadata_names : string list ref = ref []
let omit_metadata_names : string list ref = ref []
let compiled = ref false
let error_detail = ref 2
let sanity = ref false
let gc_strategy = ref Default
let force_gc = ref false
let global_timer = ref true
let experimental_field_aliasing = ref false
let ocaml_js = ref false
let rts_stack_pages_default = 32 (* 2MB *)
let rts_stack_pages : int option ref = ref None
let rtti = ref false
let trap_on_call_error = ref false
let use_stable_regions = ref false
let enhanced_orthogonal_persistence = ref false
let share_code = ref false
let stabilization_instruction_limit_default = {
  upgrade = 180_000_000_000; (* 200 billion limit with 10% reserve *)
  update_call = 18_000_000_000; (* 20 billion limit with 10% reserve *)
}
let stabilization_instruction_limit = ref stabilization_instruction_limit_default
let stable_memory_access_limit_default = 
  let gigabyte = 1024 * 1024 * 1024 in {
  upgrade = 6 * gigabyte; (* 8 GB limit with 2 GB reserves *)
  update_call = 1 * gigabyte; (* 2 GB limit with 1 GB reserve *)
}
let stable_memory_access_limit = ref stable_memory_access_limit_default
let experimental_stable_memory_default = 0 (* _ < 0: error; _ = 0: warn, _ > 0: allow *)
let experimental_stable_memory = ref experimental_stable_memory_default
