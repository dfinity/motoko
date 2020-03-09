// RUN: llvm-dwarfdump %.wasm -debug-info | tee /dev/tty | FileCheck %.mo -check-prefix=DWARF

type WeekDay = { #Mon; #Tue; #Wed; #Thu; #Fri; #Sat; #Sun };

// DWARF:  DW_TAG_enumeration_type
// DWARF:    DW_TAG_enumerator
// DWARF-NEXT: DW_AT_name         ("a")
// DWARF-NEXT: DW_AT_const_value  (0x00000000)
// DWARF:    DW_TAG_enumerator
// DWARF-NEXT: DW_AT_name         ("b")
// DWARF-NEXT: DW_AT_const_value  (0x00000001)

func variantToText(foo : {#a; #b}) : Text =
    switch foo {
    case (#a or #b)
         "whatever"
    };

// DWARF-LABEL:  DW_AT_name ("variantToText")

func dayToText(day : WeekDay) : Text =
    switch day {
    case (#Mon or #Tue or #Wed or #Thu or #Fri or #Sat or #Sun)
         "a daay :-)"
    }

// DWARF-LABEL:  DW_AT_name ("dayToText")
// DWARF:          DW_TAG_formal_parameter
// DWARF-NEXT:       DW_AT_name ("day")
// DWARF:            DW_AT_type (0x{{[0-9a-f]*}} "HUH??&")
