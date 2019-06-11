type stat_env = Typing_idl.scope
type env = stat_env

val initial_stat_env : stat_env
val initial_env      : env

type parse_result = Syntax_idl.prog Diag.result
val parse_file   : string -> parse_result

type check_result = (Syntax_idl.prog * Typing_idl.scope) Diag.result
val check_file   : string -> check_result

type ir_result = (Buffer.t, Diag.messages) result
val compile_file   : string -> ir_result
  
type compile_js_result = (Buffer.t, Diag.messages) result
val compile_js_file   : string -> compile_js_result
