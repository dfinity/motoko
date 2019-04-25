type stat_env = Typing_idl.scope
type env = stat_env

val initial_stat_env : stat_env
val initial_env      : env

type parse_result = Syntax_idl.prog Diag.result
val parse_file   : string -> parse_result

type check_result = (Syntax_idl.prog * Typing_idl.scope) Diag.result
val check_file   : string -> check_result
  (*
val check_files  : stat_env -> string list -> check_result
val check_string : stat_env -> string -> string -> check_result
val check_lexer  : stat_env -> Lexing.lexbuf -> string -> check_result

type interpret_result =
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option
val interpret_file   : env -> string -> interpret_result
val interpret_files  : env -> string list -> interpret_result
val interpret_string : env -> string -> string -> interpret_result
val interpret_lexer  : env -> Lexing.lexbuf -> string -> interpret_result

type run_result = env option
val run_file   : env -> string -> run_result
val run_files  : env -> string list -> run_result
val run_string : env -> string -> string -> run_result
val run_lexer  : env -> Lexing.lexbuf -> string -> run_result
val run_stdin  : env -> unit

type compile_mode = WasmMode | DfinityMode
type compile_result = (CustomModule.extended_module, Diag.messages) result
val compile_file   : compile_mode -> string -> string -> compile_result
val compile_files  : compile_mode -> string list -> string -> compile_result
val compile_string : compile_mode -> string -> string -> compile_result
 *)
