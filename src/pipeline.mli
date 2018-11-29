type stat_env = Typing.scope
type dyn_env = Interpret.env
type env = stat_env * dyn_env

type message = Source.region * string * Severity.t * string
type messages = message list
val print_messages : messages -> unit

val initial_stat_env : stat_env
val initial_dyn_env  : dyn_env
val initial_env      : env

type parse_result = (Syntax.prog, message) result
val parse_file   : string -> parse_result
val parse_files  : string list -> parse_result
val parse_string : string -> string -> parse_result
val parse_lexer  : Lexing.lexbuf -> string -> parse_result

type check_result = (Syntax.prog * Type.typ * Typing.scope * messages, messages) result
val check_file   : stat_env -> string -> check_result
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
type compile_result = (CustomModule.extended_module, message list) result
val compile_file   : compile_mode -> string -> compile_result
val compile_files  : compile_mode -> string list -> compile_result
val compile_string : compile_mode -> string -> string -> compile_result
