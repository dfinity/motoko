type mode = WasmMode | DfinityMode

val compile : mode -> string -> Syntax.prog -> Syntax.prog list -> CustomModule.extended_module
