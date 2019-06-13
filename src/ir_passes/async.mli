(* lower uses of async type appropriately *)
open As_ir

val transform : As_types.Scope.scope -> Ir.prog -> Ir.prog
