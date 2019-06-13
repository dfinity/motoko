type mode = WasmMode | DfinityMode

open As_ir

val compile : mode -> string -> Wasm_exts.CustomModule.extended_module option -> Ir.prog -> Ir.prog list -> Wasm_exts.CustomModule.extended_module
