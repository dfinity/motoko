val check_file  : string -> unit Diag.result
val check_string : string -> string -> unit Diag.result

val run_file           : string -> unit option
val interpret_ir_file  : string -> unit option
val run_file_and_stdin : string option -> unit option

type compile_mode = WasmMode | DfinityMode
type compile_result = CustomModule.extended_module Diag.result
val compile_string : compile_mode -> string -> string -> compile_result
val compile_file : compile_mode -> bool -> string -> compile_result
