open Ir_def

type mode = WasmMode | WASIMode | AncientMode | ICMode

val compile : mode -> string -> Wasm_exts.CustomModule.extended_module option -> Ir.prog -> Ir.prog list -> Wasm_exts.CustomModule.extended_module
