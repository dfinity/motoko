val encode : CustomModule.extended_module -> string * string

val is_dwarf_like : Wasm.Source.region -> bool
val is_dwarf_statement : Ast.instr' -> bool

val allocate_reference_slot : unit -> int
