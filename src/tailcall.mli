(* Optimize (self) tail calls to jumps, avoiding stack overflow,
   in a single linear pass *)

val transform: Typing.scope -> Ir.prog -> Ir.prog
