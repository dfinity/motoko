// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF

type Person = { name : Text; age : Nat };

// DWARF:  DW_TAG_structure_type
// DWARF:    DW_AT_name ("@obj")
// DWARF:    DW_TAG_member
// DWARF-NEXT: DW_AT_name ("age")
// DWARF-NEXT: DW_AT_type (0x{{[0-9a-f]*}} "Nat")
// XDWARF-NEXT: DW_AT_const_value  (0x00000061)
// DWARF:    DW_TAG_member
// DWARF-NEXT: DW_AT_name         ("name")
// DWARF-NEXT: DW_AT_type (0x{{[0-9a-f]*}} "Text")
// XDWARF-NEXT: DW_AT_const_value  (0x00000062)


// DWARF:      DW_TAG_typedef
// DWARF-NEXT:   DW_AT_name ("Person/1")
// DWARF-NEXT:   DW_AT_type (0x{{[0-9a-f]*}} "@obj")

func indirect(p : Person) : Person = p;

// DWARF:      DW_TAG_subprogram
// DWARF-LABEL:  DW_AT_name ("indirect")
// DWARF-NEXT:   DW_AT_type (0x{{[0-9a-f]*}} "Person/1")
// DWARF:        DW_TAG_formal_parameter
// DWARF-NEXT:     DW_AT_name ("p")

func direct({ name : Text; age : Nat }) : Text =
         "b person";

// DWARF:      DW_TAG_subprogram
// DWARF-LABEL:  DW_AT_name ("direct")
// DWARF-NEXT:   DW_AT_type (0x{{[0-9a-f]*}} "Text")
// DWARF:        DW_TAG_formal_parameter
// DWARF-NEXT:     DW_AT_name ("$param/0")
// DWARF:          DW_AT_type (0x{{[0-9a-f]*}} "@obj")

ignore direct { name = "Jane"; age = 32 };
ignore indirect { name = "Jane"; age = 32 }
