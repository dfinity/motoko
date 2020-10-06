// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF


func closure(a : Nat16) : Nat16 {
  func inner() : Nat16 = a;
  inner()
};

// DWARF:       DW_TAG_subprogram
// DWARF-LABEL:   DW_AT_name ("closure")
// DWARF:         DW_AT_decl_line (5)

// DWARF:         DW_TAG_formal_parameter
// DWARF-NEXT:      DW_AT_name ("a")
// DWARF:           DW_AT_decl_line (5)
// DWARF-NEXT:      DW_AT_decl_column (0x0d)
// DWARF:           DW_AT_type (0x{{[0-9a-f]*}} "Nat16")

// DWARF:           DW_TAG_lexical_block
// DWARF:             DW_AT_decl_line (6)
// DWARF-NEXT:        DW_AT_decl_column (0x02)

// DWARF:             DW_TAG_variable
// DWARF-NEXT:          DW_AT_name ("inner")
// DWARF-NEXT:          DW_AT_decl_line	(6)
// DWARF-NEXT:          DW_AT_decl_column (0x02)
// DWARF-NEXT:          DW_AT_type (0x{{[0-9a-f]*}} "Any")


func closureB(a : Int16) : Int16 {
  let b : Int16 = a + 42;
  func innerB() : Int16 = b;
  innerB()
};

// DWARF:       DW_TAG_subprogram
// DWARF-LABEL:   DW_AT_name ("closureB")
// DWARF:         DW_AT_decl_line (31)


func closureC(a : Word16) : Word16 {
  var c : Word16 = a + 42;
  func innerC() : Word16 = c;
  innerC()
}

// DWARF:       DW_TAG_subprogram
// DWARF-LABEL:   DW_AT_name ("closureC")
// DWARF:         DW_AT_decl_line (42)

// DWARF:           DW_AT_name ("c")
// DWARF-NEXT:      DW_AT_decl_line (43)
// DWARF-NEXT:      DW_AT_decl_column (0x02)
// DWARF-NEXT:      DW_AT_type (0x{{[0-9a-f]*}} "Word16")


// Now come the inner functions in reversed order


// DWARF:       DW_TAG_subprogram
// DWARF:         DW_AT_name ("innerC")
// DWARF:         DW_AT_decl_line	(44)
// DWARF-NEXT:    DW_AT_decl_column	(0x02)

// DWARF:         DW_TAG_lexical_block
// DWARF:           DW_AT_decl_line (44)
// DWARF-NEXT:      DW_AT_decl_column (0x02)

// DWARF:           DW_TAG_variable
// DWARF-NEXT:        DW_AT_name ("c")
// DWARF-NEXT:        DW_AT_decl_line (43)
// DWARF-NEXT:        DW_AT_decl_column (0x02)
// DWARF-NEXT:        DW_AT_type (0x{{[0-9a-f]*}} "Word16")



// DWARF:       DW_TAG_subprogram
// DWARF:         DW_AT_name ("innerB")
// DWARF:         DW_AT_decl_line	(33)
// DWARF-NEXT:    DW_AT_decl_column	(0x02)

// DWARF:         DW_TAG_lexical_block
// DWARF:           DW_AT_decl_line (33)
// DWARF-NEXT:      DW_AT_decl_column (0x02)

// DWARF:           DW_TAG_variable
// DWARF-NEXT:        DW_AT_name ("b")
// DWARF-NEXT:        DW_AT_decl_line (32)
// DWARF-NEXT:        DW_AT_decl_column (0x06)
// DWARF-NEXT:        DW_AT_type (0x{{[0-9a-f]*}} "Int16")



// DWARF:       DW_TAG_subprogram
// DWARF:         DW_AT_name ("inner")
// DWARF:         DW_AT_decl_line	(6)
// DWARF-NEXT:    DW_AT_decl_column	(0x02)

// DWARF:         DW_TAG_lexical_block
// DWARF:           DW_AT_decl_line (6)
// DWARF-NEXT:      DW_AT_decl_column (0x02)

// DWARF:           DW_TAG_variable
// DWARF-NEXT:        DW_AT_name ("a")
// DWARF-NEXT:        DW_AT_decl_line (5)
// DWARF-NEXT:        DW_AT_decl_column (0x0d)
// DWARF-NEXT:        DW_AT_type (0x{{[0-9a-f]*}} "Nat16")

//MOC-FLAG -g
