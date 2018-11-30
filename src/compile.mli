type mode = WasmMode | DfinityMode

val compile : string -> mode -> Syntax.prog -> Syntax.prog list -> CustomModule.extended_module
