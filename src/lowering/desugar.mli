open As_frontend
open As_ir

val transform : Syntax.prog -> Ir.prog
val transform_graph : As_types.Scope.lib_env -> Syntax.libraries -> Syntax.prog list -> Ir.prog
