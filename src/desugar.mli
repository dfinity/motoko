val transform_prelude : Syntax.prog -> Ir.dec list list
val transform : Syntax.prog -> Ir.prog
val transform_graph : Typing.lib_env -> Syntax.libraries -> Syntax.prog list -> Ir.prog
