(* common flags for the moc compiler *)

type compile_mode = WasmMode | AncientMode | ICMode

let trace = ref false
let verbose = ref false
let print_warnings = ref true
let print_depth = ref 2
let release_mode = ref false
let compile_mode = ref ICMode
let multi_value = ref false
let await_lowering = ref true
let async_lowering = ref true
let dump_parse = ref false
let dump_tc = ref false
let dump_lowering = ref false
let check_ir = ref true
let package_urls : (string * string) list ref = ref []
let profile = ref false
let profile_verbose = ref false
let profile_file = ref "profiling-counters.csv"
let profile_line_prefix = ref ""
let profile_field_names : string list ref = ref []
let compiled = ref false
