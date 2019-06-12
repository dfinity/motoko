(* lower uses of async type appropriately *)
open As_frontend
open As_ir

val transform : Typing.scope -> Ir.prog -> Ir.prog
