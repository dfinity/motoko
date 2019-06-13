open As_frontend
open As_ir

val transform : Syntax.prog -> Ir.prog
val transform_graph : Typing.lib_env -> Syntax.libraries -> Syntax.prog list -> Ir.prog
