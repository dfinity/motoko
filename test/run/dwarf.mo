// RUN: llvm-dwarfdump %.wasm -debug-abbrev -debug-info --verbose | FileCheck %.mo -check-prefix=DWARF

// DWARF: dwarf.wasm: file format WASM

// DWARF: .debug_abbrev contents:
// DWARF-NEXT: Abbrev table for offset: 0x00000000
// DWARF-NEXT: [1] DW_TAG_compile_unit DW_CHILDREN_yes
// DWARF-NEXT:         DW_AT_producer  DW_FORM_strp
// DWARF-NEXT:         DW_AT_language  DW_FORM_data2
// DWARF-NEXT:         DW_AT_name      DW_FORM_strp
// DWARF-NEXT:         DW_AT_stmt_list DW_FORM_sec_offset
// DWARF-NEXT:         DW_AT_comp_dir  DW_FORM_strp
// DWARF-NEXT:         DW_AT_use_UTF8  DW_FORM_flag_present
// DWARF-NEXT:         DW_AT_low_pc    DW_FORM_addr
// DWARF-NEXT:         DW_AT_addr_base DW_FORM_sec_offset
// DWARF-NEXT:         DW_AT_ranges    DW_FORM_sec_offset

import Prim "mo:prim"

assert (1 == 1);
// DWARF-LABEL: .debug_info contents:
// DWARF:          : DW_TAG_compile_unit [1] *
// DWARF-NEXT:         DW_AT_producer [DW_FORM_strp] {{.*}} "DFINITY Motoko compiler, revision {{.*}}"
// DWARF-NEXT:         DW_AT_language [DW_FORM_data2] (0x0027)
// DWARF-NEXT:         DW_AT_name     [DW_FORM_strp] {{.*}} "dwarf.mo.mangled"
// DWARF-NEXT:         DW_AT_stmt_list [DW_FORM_sec_offset] (0x00000000)
// DWARF-NEXT:         DW_AT_comp_dir [DW_FORM_strp] {{.*}} "."
// DWARF-NEXT:         DW_AT_use_UTF8 [DW_FORM_flag_present] (true)
// DWARF-NEXT:         DW_AT_low_pc   [DW_FORM_addr] (0x0000000000000000)
// DWARF-NEXT:         DW_AT_addr_base  [DW_FORM_sec_offset] (0x00000008)
// DWARF-NEXT:         DW_AT_ranges   [DW_FORM_sec_offset] (0x0000000c
// DWARF:           NULL

func foo (a : Int) : Bool {
  return a == 42
};

// DWARF:          : DW_TAG_subprogram [3] *
// DWARF-LABEL:        DW_AT_name     [DW_FORM_strp] {{.*}} "foo"
// DWARF-LABEL:        DW_AT_type     [DW_FORM_ref_udata] (cu + {{0x.*}} => {{.*}} "Bool")
// DWARF-NEXT:         DW_AT_decl_file [DW_FORM_data1] ("./dwarf.mo.mangled")
// DWARF-NEXT:         DW_AT_decl_line [DW_FORM_data1] (
// DWARF-NEXT:         DW_AT_decl_column [DW_FORM_data1]      (0x00)

// DWARF:          :   DW_TAG_formal_parameter [4]
// DWARF-NEXT:           DW_AT_name     [DW_FORM_strp] {{.*}} "a"
// DWARF-NEXT:           DW_AT_decl_line [DW_FORM_data1] (
// DWARF-NEXT:           DW_AT_decl_column [DW_FORM_data1] (0x0a)
// DWARF-NEXT:           DW_AT_type [DW_FORM_ref_udata]	(cu + {{.*}} => {{.*}} "Int")
// DWARF-NEXT:           DW_AT_location [DW_FORM_exprloc] (DW_OP_WASM_location 0x0 +1, DW_OP_lit1, DW_OP_shra, DW_OP_stack_value)
// DWARF:          NULL
// DWARF:        NULL

func baz (a : Int) : Bool = a == 42;

func bar (a : Int) : Int {
    let b = a + 42;

    if (foo b) { b } else { ignore b; assert (a != 42); b }
};

assert baz(42) == true;
assert bar(0) == 42;

//MOC-FLAG -g
