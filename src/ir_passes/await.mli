(* lower uses of async and await to continuation passing style *)
open Ir_def

val transform : Ir.prog -> Ir.prog
