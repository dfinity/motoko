// RUN: llvm-dwarfdump %.wasm -debug-info | FileCheck %.mo -check-prefix=DWARF

// DWARF:  DW_TAG_enumeration_type
// DWARF:    DW_TAG_enumerator
// DWARF-NEXT: DW_AT_name ("Fri")
// DWARF:    NULL

type WeekDay = { #Mon; #Tue; #Wed; #Thu; #Fri; #Sat; #Sun };

// DWARF:  DW_TAG_enumeration_type
// DWARF:    DW_TAG_enumerator
// DWARF-NEXT: DW_AT_name ("a")
// DWARF-NEXT: DW_AT_const_value (0x00000061)
// DWARF:    DW_TAG_enumerator
// DWARF-NEXT: DW_AT_name ("b")
// DWARF-NEXT: DW_AT_const_value (0x00000062)
// DWARF:    NULL

func variantToText(foo : {#a; #b}) : Text =
    switch foo {
    case (#a or #b)
         "whatever"
    };

// DWARF-LABEL:  DW_AT_name ("variantToText")
// DWARF:          DW_TAG_formal_parameter
// DWARF-NEXT:       DW_AT_name ("foo")
// DWARF-NEXT:       DW_AT_decl_line (19)
// DWARF-NEXT:       DW_AT_decl_column (0x13)
// DWARF-NEXT:       DW_AT_type (0x{{[0-9a-f]*}} "enumeration ")
// DWARF-NEXT:       DW_AT_location (DW_OP_WASM_location 0x0 +1, DW_OP_plus_uconst 0x5, DW_OP_deref, DW_OP_stack_value)
// DWARF:          NULL


func dayToText(day : WeekDay) : Text =
    switch day {
    case (#Mon or #Tue or #Wed or #Thu or #Fri or #Sat or #Sun)
         "a daay :-)"
    };

// DWARF-LABEL:  DW_AT_name ("dayToText")
// DWARF:          DW_TAG_formal_parameter
// DWARF-NEXT:       DW_AT_name ("day")
// DWARF:            DW_AT_type (0x{{[0-9a-f]*}} "enumeration "
// DWARF-NEXT:       DW_AT_location (DW_OP_WASM_location 0x0 +1, DW_OP_plus_uconst 0x5, DW_OP_deref, DW_OP_stack_value)
// DWARF:          NULL


ignore dayToText (#Fri);
ignore variantToText (#a)

//MOC-FLAG -g
