// RUN: llvm-dwarfdump %.wasm -debug-info | tee /dev/tty | FileCheck %.mo -check-prefix=DWARF

type Person = { name : Text; age : Nat };

// DWARF:  DW_TAG_structure_type
// DWARF:    DW_AT_name ("@obj")
// DWARF:    DW_TAG_member
// DWARF-NEXT: DW_AT_name         ("age")
// XDWARF-NEXT: DW_AT_const_value  (0x00000061)
// DWARF:    DW_TAG_member
// DWARF-NEXT: DW_AT_name         ("name")
// XDWARF-NEXT: DW_AT_const_value  (0x00000062)
// DWARF:  DW_TAG_reference_type

func personToText(p : Person) : Text =
         "a person";

// DWARF:      DW_TAG_subprogram
// DWARF-LABEL:  DW_AT_name ("personToText")
// DWARF:          DW_TAG_formal_parameter
// DWARF-NEXT:       DW_AT_name ("p")

func direct({ name : Text; age : Nat }) : Text =
         "b person";

// DWARF:      DW_TAG_subprogram
// DWARF-LABEL:  DW_AT_name ("direct")
// DWARF:          DW_TAG_formal_parameter
// DWARF-NEXT:       DW_AT_name ("$param/0")
// DWARF:            DW_AT_type (0x{{[0-9a-f]*}} "@obj&")
