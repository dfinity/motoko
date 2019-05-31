val check_files  : string list -> unit Diag.result
val check_string : string -> string -> unit Diag.result

val run_files           : string list -> unit option
val interpret_ir_files  : string list -> unit option
val run_files_and_stdin : string list -> unit option

type compile_mode = WasmMode | DfinityMode
type compile_result = CustomModule.extended_module Diag.result
val compile_string : compile_mode -> string -> string -> compile_result
val compile_files : compile_mode -> bool -> string list -> compile_result
