type mode = WasmMode | DfinityMode

val compile : mode -> Syntax.prog -> Syntax.prog list -> CustomModule.extended_module
