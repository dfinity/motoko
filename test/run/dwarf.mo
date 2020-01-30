// RUN: /nix/store/qns5jqkwnrbfj10z2n03c67ljy6a2n2j-llvm-10.0.0-branch/bin/llvm-dwarfdump %s -all | FileCheck %s -check-prefix=DWARF

// CHECK-DWARF: charToText.wasm:        file format WASM

// DWARF: .debug_abbrev contents:
// DWARF: Abbrev table for offset: 0x00000000
// DWARF-NEXT: [1] DW_TAG_compile_unit DW_CHILDREN_yes
// DWARF-NEXT:         DW_AT_producer  DW_FORM_strp
// DWARF-NEXT:         DW_AT_language  DW_FORM_data2
// DWARF-NEXT:         DW_AT_name      DW_FORM_strp
// DWARF-NEXT:         DW_AT_stmt_list DW_FORM_sec_offset
// DWARF-NEXT:         DW_AT_comp_dir  DW_FORM_strp
// DWARF-NEXT:         DW_AT_low_pc    DW_FORM_addr
// DWARF-NEXT:         DW_AT_high_pc   DW_FORM_data4

import Prim "mo:prim"

assert (1 == 1)

// CHECK: to trigger the test
