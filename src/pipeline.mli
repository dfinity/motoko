type stat_env = Typing.env
type dyn_env = Interpret.env
type env = stat_env * dyn_env

val init : unit -> env

type parse_result = Syntax.prog
val parse_file   : string -> Syntax.prog option
val parse_files  : string list -> Syntax.prog option
val parse_string : string -> string -> parse_result option
val parse_lexer  : Lexing.lexbuf -> string -> parse_result option

type check_result = Syntax.prog * Type.typ * Typing.scope
val check_file   : stat_env -> string -> check_result option
val check_files  : stat_env -> string list -> check_result option
val check_string : stat_env -> string -> string -> check_result option
val check_lexer  : stat_env -> Lexing.lexbuf -> string -> check_result option

type interpret_result =
  Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope
val interpret_file   : env -> string -> interpret_result option
val interpret_files  : env -> string list -> interpret_result option
val interpret_string : env -> string -> string -> interpret_result option
val interpret_lexer  : env -> Lexing.lexbuf -> string -> interpret_result option

type run_result = env
val run_file   : env -> string -> run_result option
val run_files  : env -> string list -> run_result option
val run_string : env -> string -> string -> run_result option
val run_lexer  : env -> Lexing.lexbuf -> string -> run_result option

type compile_result = Wasm.Ast.module_
val compile_file   : string -> compile_result option
val compile_files  : string list -> compile_result option
val compile_string : string -> string -> compile_result option

val run_stdin : env -> unit
