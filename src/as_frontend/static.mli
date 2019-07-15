open As_def

val exp : Diag.msg_store -> Syntax.exp -> unit
val fields : Diag.msg_store -> Syntax.exp_field list -> unit
val prog : Syntax.prog -> unit Diag.result
