(* Optimize local occurrences of identical values rebuilt from
   bindings after a pattern match *)

open Ir_def

val transform: Ir.prog -> Ir.prog
