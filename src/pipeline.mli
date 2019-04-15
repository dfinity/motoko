type stat_env = Typing.scope
type dyn_env = Interpret.scope
type env = stat_env * dyn_env

val initial_stat_env : stat_env
val initial_dyn_env  : dyn_env
val initial_env      : env

type check_result = (Syntax.prog * Type.typ * Typing.scope) Diag.result
val check_files  : stat_env -> string list -> check_result
val check_string : stat_env -> string -> string -> check_result

type run_result = env option
val run_files  : env -> string list -> run_result
val run_stdin  : env -> unit

type compile_mode = WasmMode | DfinityMode
type compile_result = (CustomModule.extended_module, Diag.messages) result
val compile_string : compile_mode -> string -> string -> compile_result
val compile_files : compile_mode -> string list -> string -> compile_result
