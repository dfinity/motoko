val check_file   : string -> (Syntax.prog * Typing.scope) Diag.result

val compile_js_file   : string -> Buffer.t Diag.result
