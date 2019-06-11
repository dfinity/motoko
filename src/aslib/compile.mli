type mode = WasmMode | DfinityMode

val compile : mode -> string -> CustomModule.extended_module option -> Ir.prog -> Ir.prog list -> CustomModule.extended_module
