type mode = WasmMode | DfinityMode

val compile : mode -> string -> CustomModule.extended_module option -> Ir.dec list list -> Ir.prog -> CustomModule.extended_module
