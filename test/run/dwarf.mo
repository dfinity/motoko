// RUN: llvm-dwarfdump %.wasm -all | FileCheck %.mo -check-prefix=DWARF

// DWARF: dwarf.wasm: file format WASM

// DWARF: .debug_abbrev contents:
// DWARF-NEXT: Abbrev table for offset: 0x00000000
// DWARF-NEXT: [1] DW_TAG_compile_unit DW_CHILDREN_yes
// DWARF-NEXT:         DW_AT_producer  DW_FORM_strp
// DWARF-NEXT:         DW_AT_language  DW_FORM_data2
// DWARF-NEXT:         DW_AT_name      DW_FORM_strp
// DXXXXXWARF-NEXT:         DW_AT_stmt_list DW_FORM_sec_offset
// DWARF-NEXT:         DW_AT_comp_dir  DW_FORM_strp
// DWARF-NEXT:         DW_AT_low_pc    DW_FORM_addr
// DWARF-NEXT:         DW_AT_high_pc   DW_FORM_data4

import Prim "mo:prim"

assert (1 == 1)

// DWARF:            DW_TAG_compile_unit
// DWARF-NEXT:         DW_AT_producer
// DWARF-NEXT:         DW_AT_language
// DWARF-NEXT:         DW_AT_name
// DWARF-NEXT:         DW_AT_comp_dir
// DWARF-NEXT:         DW_AT_low_pc
// DWARF-NEXT:         DW_AT_high_pc
