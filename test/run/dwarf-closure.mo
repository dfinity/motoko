// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF



func closure(a : Nat16) : Nat16 {
  func inner() : Nat16 = a;
  inner()
}

// DWARF:       DW_TAG_subprogram
// DWARF-LABEL:   DW_AT_name ("closure")
// DWARF:         DW_AT_decl_line (5)

// DWARF:         DW_TAG_formal_parameter
// DWARF-NEXT:      DW_AT_name ("a")

// DWARF:           DW_TAG_lexical_block
// DWARF:             DW_AT_decl_line (6)
// DWARF-NEXT:        DW_AT_decl_column (0x02)

// DWARF:             DW_TAG_variable
// DWARF-NEXT:          DW_AT_name ("inner")
// DWARF-NEXT:          DW_AT_decl_line	(0)
// DWARF-NEXT:          DW_AT_decl_column (0x00)


// DWARF:       DW_TAG_subprogram
// DWARF:         DW_AT_name ("inner")
// DWARF:         DW_AT_decl_line	(6)
// DWARF-NEXT:    DW_AT_decl_column	(0x02)

// DWARF:         DW_TAG_lexical_block
// DWARF:           DW_AT_decl_line (6)
// DWARF-NEXT:      DW_AT_decl_column (0x02)

// DWARF:           DW_TAG_variable
// DWARF-NEXT:        DW_AT_name ("a")
// DWARF-NEXT:        DW_AT_decl_line (0)
// DWARF-NEXT:        DW_AT_decl_column (0x00)
// DWARF:             DW_AT_type (0x{{[0-9a-f]*}} "Any")
