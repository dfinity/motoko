type mode = WasmMode | DfinityMode

val compile : mode -> string -> Ir.prog -> Ir.prog list -> CustomModule.extended_module
