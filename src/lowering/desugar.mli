open Mo_def
open Ir_def

val transform : Syntax.prog -> Ir.prog
val transform_graph : Syntax.lib list -> Syntax.prog list -> Ir.prog
