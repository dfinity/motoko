(* lower uses of async type appropriately *)
open Ir_def

type platform =
    V1  (* legacy, Haskell *)
  | V2  (* new, Rust *)

val transform : platform -> As_types.Scope.scope -> Ir.prog -> Ir.prog
