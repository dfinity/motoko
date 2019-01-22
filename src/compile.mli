type mode = WasmMode | DfinityMode

type config =
  { mode : mode;
    module_name : string;
    source_mapping_url : string option;
  }

val compile : config -> Ir.prog -> Ir.prog list -> CustomModule.extended_module
