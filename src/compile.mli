type mode = WasmMode | DfinityMode

val compile : mode -> string -> Ir.prog -> Ir.prog list -> Wasm_copy.CustomModule.extended_module
