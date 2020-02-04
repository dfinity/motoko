(* lower uses of async type appropriately *)
open Ir_def

val transform :
  Mo_config.Flags.compile_mode -> Mo_types.Scope.scope -> Ir.prog -> Ir.prog
