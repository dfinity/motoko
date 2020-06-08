val encode : CustomModule.extended_module -> string * string

val is_dwarf_like : Ast.instr' -> bool

val allocate_reference_slot : unit -> int
