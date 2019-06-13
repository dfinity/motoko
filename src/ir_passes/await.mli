(* lower uses of async and await to continuation passing style *)
open As_ir
val transform : Ir.prog -> Ir.prog
