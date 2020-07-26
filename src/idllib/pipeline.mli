val check_file : string -> (Syntax.prog * Typing.scope * Syntax.typ option) Diag.result
val check_prog : Syntax.prog -> Typing.scope Diag.result
val compile_js_string : string -> Buffer.t Diag.result
val compile_js_file : string -> Buffer.t Diag.result
