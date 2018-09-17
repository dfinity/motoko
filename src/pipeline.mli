type stat_env = Typing.env
type dyn_env = Interpret.env
type env = stat_env * dyn_env

val init : unit -> env

type parse_result = Syntax.prog
val parse_file   : string -> Syntax.prog option
val parse_string : string -> string -> parse_result option
val parse_lexer  : Lexing.lexbuf -> string -> parse_result option

type check_result = Syntax.prog * Type.typ * Typing.scope
val check_file   : stat_env -> string -> check_result option
val check_string : string -> stat_env -> string -> check_result option
val check_lexer  : Lexing.lexbuf -> stat_env -> string -> check_result option

type interpret_result =
  Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope
val interpret_file   : env -> string -> interpret_result option
val interpret_string : string -> env -> string -> interpret_result option
val interpret_lexer  : Lexing.lexbuf -> env -> string -> interpret_result option

type run_result = env
val run_file   : env -> string -> run_result option
val run_string : string -> env -> string -> run_result option
val run_lexer  : Lexing.lexbuf -> env -> string -> run_result option

type compile_result = Wasm.Ast.module_ * Typing.scope
val compile_file   : stat_env -> string -> compile_result option
val compile_string : string -> stat_env -> string -> compile_result option

val run_stdin : env -> unit
