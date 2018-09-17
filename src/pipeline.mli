val init : unit -> Typing.env * Interpret.env

val parse_string : string -> string -> Syntax.prog option
val parse_file : string -> Syntax.prog option
val parse_lexer : Lexing.lexbuf -> string -> Syntax.prog option

val check_string : string -> Typing.env -> string ->
  (Syntax.prog * Type.typ * Typing.scope) option
val check_file : Typing.env -> string ->
  (Syntax.prog * Type.typ * Typing.scope) option
val check_lexer : Lexing.lexbuf -> Typing.env -> string ->
  (Syntax.prog * Type.typ * Typing.scope) option

val interpret_string : string -> Typing.env * Interpret.env -> string ->
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option
val interpret_file : Typing.env * Interpret.env -> string ->
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option
val interpret_lexer : Lexing.lexbuf -> Typing.env * Interpret.env -> string ->
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option

val run_string : string -> Typing.env * Interpret.env -> string ->
  (Typing.env * Interpret.env) option
val run_file : Typing.env * Interpret.env -> string ->
  (Typing.env * Interpret.env) option
val run_lexer : Lexing.lexbuf -> Typing.env * Interpret.env -> string ->
  (Typing.env * Interpret.env) option

val compile_string : string -> Typing.env -> string ->
  (Wasm.Ast.module_ * Typing.scope) option
val compile_file : Typing.env -> string ->
  (Wasm.Ast.module_ * Typing.scope) option

val run_stdin : Typing.env * Interpret.env -> unit
