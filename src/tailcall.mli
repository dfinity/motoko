(* Optimize (self) tail calls to jumps, avoiding stack overflow,
   in a single linear pass *)

val transform: Ir.prog -> Ir.prog
