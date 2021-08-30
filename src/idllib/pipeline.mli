val check_file : string -> (Syntax.prog * Typing.scope * Syntax.typ option) Diag.result
val check_prog : Syntax.prog -> Typing.scope Diag.result

val parse_test_file : string -> Syntax.tests Diag.result
val parse_values : string -> Syntax.args Diag.result
