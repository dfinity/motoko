(* lower uses of async type appropriately *)
open Ir_def

val transform : As_types.Scope.scope -> Ir.prog -> Ir.prog
