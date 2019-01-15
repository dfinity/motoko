type mode = WasmMode | DfinityMode

val compile : mode -> string -> string option -> Ir.prog -> Ir.prog list -> CustomModule.extended_module
