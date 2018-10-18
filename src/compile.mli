type mode = WasmMode | DfinityMode

val compile : mode -> Syntax.prog -> Syntax.prog list -> Wasm.Ast.module_
