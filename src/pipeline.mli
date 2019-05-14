type mode = WasmMode | DfinityMode

val check_files  : mode -> string list -> unit Diag.result
val check_string : string -> string -> unit Diag.result

val run_files           : mode -> string list -> unit Diag.result
val interpret_ir_files  : mode -> string list -> unit Diag.result
val run_files_and_stdin : mode -> string list -> unit Diag.result

type compile_result = (CustomModule.extended_module, Diag.messages) result
val compile_string : mode -> string -> string -> compile_result
val compile_files : mode -> bool -> string list -> compile_result
