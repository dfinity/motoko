let abbreviations =
  let open Dwarf5 in
  [ ( dw_TAG_compile_unit, dw_CHILDREN_yes,
      [ dw_AT_producer, dw_FORM_strp;
        dw_AT_language, dw_FORM_data2;
        dw_AT_name, dw_FORM_strp;
        (* dw_AT_stmt_list, dw_FORM_sec_offset; *)
        dw_AT_comp_dir, dw_FORM_strp;
        dw_AT_low_pc, dw_FORM_addr;
        dw_AT_high_pc, dw_FORM_data4
      ] );
    ( dw_TAG_subprogram, dw_CHILDREN_yes,
      [ dw_AT_low_pc, dw_FORM_addr;
      dw_AT_high_pc, dw_FORM_data4;
      (* dw_AT_GNU_all_call_sites, dw_FORM_flag_present; *)
      dw_AT_name, dw_FORM_strp;
      dw_AT_decl_file, dw_FORM_data1;
      dw_AT_decl_line, dw_FORM_data1;
      dw_AT_prototyped, dw_FORM_flag_present;
      dw_AT_external, dw_FORM_flag_present
      ] );
    ( dw_TAG_formal_parameter, dw_CHILDREN_no,
      [ dw_AT_name, dw_FORM_strp;
      dw_AT_decl_file, dw_FORM_data1;
      dw_AT_decl_line, dw_FORM_data1;
      dw_AT_type, dw_FORM_ref4;
      ] )
  ]
