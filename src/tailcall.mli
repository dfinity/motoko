(* Optimize (self) tail calls to jumps, avoiding stack overflow,
   in a single linear pass *)

val prog: Syntax.prog -> Syntax.prog
