(* Optimize (self) tail calls to jumps, avoiding stack overflow,
   in a single linear pass *)

open Ir_def

val transform: Ir.prog -> Ir.prog
