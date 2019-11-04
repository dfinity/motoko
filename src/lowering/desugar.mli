open Mo_def
open Mo_types
open Ir_def

val transform : Syntax.prog -> Ir.prog
val transform_graph : Scope.lib_env -> Syntax.lib list -> Syntax.prog list -> Ir.prog
