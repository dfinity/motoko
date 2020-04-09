open Ir_def

val compile : Mo_config.Flags.compile_mode -> string -> Wasm_exts.CustomModule.extended_module option -> Ir.prog list -> Wasm_exts.CustomModule.extended_module
