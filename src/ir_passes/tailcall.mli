(* Optimize (self) tail calls to jumps, avoiding stack overflow,
   in a single linear pass *)

open As_ir

val transform: Ir.prog -> Ir.prog
