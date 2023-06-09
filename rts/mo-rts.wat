(module
  (type (;0;) (func (param i64 i64) (result i32)))
  (type (;1;) (func (param i32 i64 i64 i64 i64 i64 i64)))
  (type (;2;) (func (param i64 i32)))
  (type (;3;) (func (param i64 i64)))
  (type (;4;) (func (param i64) (result i32)))
  (type (;5;) (func (param i32 i64 i64 i64 i64)))
  (type (;6;) (func (param i64 i64 i64)))
  (type (;7;) (func (param i32) (result i32)))
  (type (;8;) (func (param i64 i64) (result i64)))
  (type (;9;) (func (result i32)))
  (type (;10;) (func))
  (type (;11;) (func (param i64)))
  (type (;12;) (func (param i64 i64 i64 i64)))
  (type (;13;) (func (param i32 i64)))
  (type (;14;) (func (param i32 i32) (result i32)))
  (type (;15;) (func (param i32 i64) (result i32)))
  (type (;16;) (func (param i32)))
  (type (;17;) (func (param i64 i64 i32)))
  (type (;18;) (func (param i32) (result i64)))
  (type (;19;) (func (param i32 i32)))
  (type (;20;) (func (result i64)))
  (type (;21;) (func (param i64 i32) (result i32)))
  (type (;22;) (func (param i64 i64 i64) (result i64)))
  (import "env" "memory" (memory (;0;) i64 1))
  (import "env" "__indirect_function_table" (table (;0;) 2 funcref))
  (import "env" "__stack_pointer" (global $__stack_pointer (mut i64)))
  (import "env" "__memory_base" (global $__memory_base i64))
  (import "env" "__table_base" (global $__table_base i64))
  (import "env" "__table_base32" (global $__table_base32 i32))
  (import "env" "_ZN4core3fmt17pointer_fmt_inner17h0f91079b1433d08fE" (func $core::fmt::pointer_fmt_inner::h0f91079b1433d08f (type 0)))
  (import "env" "_ZN4core9panicking19assert_failed_inner17ha1e4ba1819b6d71dE" (func $core::panicking::assert_failed_inner::ha1e4ba1819b6d71d (type 1)))
  (import "env" "_ZN4core7unicode12unicode_data11conversions8to_upper17h508b12c71146707bE" (func $core::unicode::unicode_data::conversions::to_upper::h508b12c71146707b (type 2)))
  (import "env" "_ZN4core4char15CaseMappingIter3new17hd01f267f074fcbfcE" (func $core::char::CaseMappingIter::new::hd01f267f074fcbfc (type 3)))
  (import "env" "_ZN82_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$9size_hint17h76de20e1c0d8be2aE" (func $_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::size_hint::h76de20e1c0d8be2a (type 3)))
  (import "env" "_ZN82_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17h7c74682c8f2bd818E" (func $_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::next::h7c74682c8f2bd818 (type 4)))
  (import "env" "_ZN4core9panicking13assert_failed17h418c82b05c5b11d1E" (func $core::panicking::assert_failed::h418c82b05c5b11d1 (type 5)))
  (import "env" "_ZN4core9panicking5panic17h1108c90903a88b24E" (func $core::panicking::panic::h1108c90903a88b24 (type 6)))
  (import "env" "_ZN4core7unicode12unicode_data11conversions8to_lower17ha7938dd9ffb4d0adE" (func $core::unicode::unicode_data::conversions::to_lower::ha7938dd9ffb4d0ad (type 2)))
  (import "env" "_ZN82_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$9size_hint17he2722b4f780a6b65E" (func $_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::size_hint::he2722b4f780a6b65 (type 3)))
  (import "env" "_ZN82_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17h5a9ec19c206cf828E" (func $_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::next::h5a9ec19c206cf828 (type 4)))
  (import "env" "_ZN4core7unicode12unicode_data9uppercase6lookup17h86d90efac1b8d8c5E" (func $core::unicode::unicode_data::uppercase::lookup::h86d90efac1b8d8c5 (type 7)))
  (import "env" "_ZN4core7unicode12unicode_data9lowercase6lookup17he4c936c8b6cc6e22E" (func $core::unicode::unicode_data::lowercase::lookup::he4c936c8b6cc6e22 (type 7)))
  (import "env" "_ZN4core7unicode12unicode_data10alphabetic6lookup17hf08775a06e39ded4E" (func $core::unicode::unicode_data::alphabetic::lookup::hf08775a06e39ded4 (type 7)))
  (import "env" "_ZN4core6option13expect_failed17h71d0f13fa0de0cdbE" (func $core::option::expect_failed::h71d0f13fa0de0cdb (type 6)))
  (import "env" "_ZN4core3str5count14do_count_chars17h0aeec386532c2f2bE" (func $core::str::count::do_count_chars::h0aeec386532c2f2b (type 8)))
  (import "env" "_ZN4core3str5count23char_count_general_case17h9ee082f639101326E" (func $core::str::count::char_count_general_case::h9ee082f639101326 (type 8)))
  (import "env" "_ZN4core5slice4iter87_$LT$impl$u20$core..iter..traits..collect..IntoIterator$u20$for$u20$$RF$$u5b$T$u5d$$GT$9into_iter17h255c1c79ea70b7e8E" (func $core::slice::iter::_$LT$impl$u20$core..iter..traits..collect..IntoIterator$u20$for$u20$$RF$$u5b$T$u5d$$GT$::into_iter::h255c1c79ea70b7e8 (type 6)))
  (import "env" "rts_trap" (func $rts_trap (type 2)))
  (import "env" "get_heap_base" (func $get_heap_base (type 9)))
  (import "env" "get_static_roots" (func $get_static_roots (type 9)))
  (import "env" "_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h5ba0ed2a49bf2e8cE" (func $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h5ba0ed2a49bf2e8c (type 0)))
  (import "GOT.mem" "_ZN4core7unicode12unicode_data11white_space14WHITESPACE_MAP17ha9af54a48f3239a7E" (global $core::unicode::unicode_data::white_space::WHITESPACE_MAP::ha9af54a48f3239a7 (mut i64)))
  (import "GOT.func" "_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h5ba0ed2a49bf2e8cE" (global $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h5ba0ed2a49bf2e8c (mut i64)))
  (func $__wasm_call_ctors (type 10)
    call $__wasm_apply_data_relocs)
  (func $__wasm_apply_data_relocs (type 10)
    global.get $__memory_base
    i64.const 3016
    i64.add
    global.get $__memory_base
    i64.const 0
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3040
    i64.add
    global.get $__table_base
    i64.const 0
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3064
    i64.add
    global.get $__table_base
    i64.const 1
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3072
    i64.add
    global.get $__table_base
    i64.const 0
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3096
    i64.add
    global.get $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h5ba0ed2a49bf2e8c
    i64.store align=4
    global.get $__memory_base
    i64.const 3104
    i64.add
    global.get $__memory_base
    i64.const 214
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3128
    i64.add
    global.get $__memory_base
    i64.const 214
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3152
    i64.add
    global.get $__memory_base
    i64.const 577
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3176
    i64.add
    global.get $__memory_base
    i64.const 577
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3200
    i64.add
    global.get $__memory_base
    i64.const 660
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3224
    i64.add
    global.get $__memory_base
    i64.const 1822
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3248
    i64.add
    global.get $__memory_base
    i64.const 1822
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3272
    i64.add
    global.get $__memory_base
    i64.const 2114
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3296
    i64.add
    global.get $__memory_base
    i64.const 2114
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3320
    i64.add
    global.get $__memory_base
    i64.const 2147
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3344
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3376
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3400
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3424
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3448
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3472
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3496
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3520
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3544
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3568
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3592
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3616
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3640
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3664
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3688
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3712
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3736
    i64.add
    global.get $__memory_base
    i64.const 2255
    i64.add
    i64.store align=4
    global.get $__memory_base
    i64.const 3760
    i64.add
    global.get $__memory_base
    i64.const 2980
    i64.add
    i64.store align=4)
  (func $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h1ed0b47d0bd87b82 (type 0) (param i64 i64) (result i32)
    local.get 0
    i64.load
    i64.load
    local.get 1
    call $core::fmt::pointer_fmt_inner::h0f91079b1433d08f)
  (func $core::ptr::drop_in_place$LT$$RF$u32$GT$::h526868480085c5e9 (type 11) (param i64))
  (func $core::panicking::assert_failed::h294badccb53d6a88 (type 3) (param i64 i64)
    (local i64)
    global.get $__stack_pointer
    i64.const 64
    i64.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i64.store offset=8
    local.get 2
    global.get $__memory_base
    local.tee 0
    i64.const 3816
    i64.add
    i64.store
    local.get 2
    i64.const 16
    i64.add
    local.get 1
    i64.const 48
    memory.copy
    i32.const 0
    local.get 2
    local.get 0
    i64.const 3040
    i64.add
    local.tee 1
    local.get 2
    i64.const 8
    i64.add
    local.get 1
    local.get 2
    i64.const 16
    i64.add
    local.get 0
    i64.const 3296
    i64.add
    call $core::panicking::assert_failed_inner::ha1e4ba1819b6d71d
    unreachable)
  (func $core::panicking::assert_failed::ha6cca40bd32447e3 (type 12) (param i64 i64 i64 i64)
    (local i64)
    global.get $__stack_pointer
    i64.const 64
    i64.sub
    local.tee 4
    global.set $__stack_pointer
    local.get 4
    local.get 1
    i64.store offset=8
    local.get 4
    local.get 0
    i64.store
    local.get 4
    i64.const 16
    i64.add
    local.get 2
    i64.const 48
    memory.copy
    i32.const 0
    local.get 4
    global.get $__memory_base
    i64.const 3072
    i64.add
    local.tee 1
    local.get 4
    i64.const 8
    i64.add
    local.get 1
    local.get 4
    i64.const 16
    i64.add
    local.get 3
    call $core::panicking::assert_failed_inner::ha1e4ba1819b6d71d
    unreachable)
  (func $blob_iter_done (type 7) (param i32) (result i32)
    (local i64)
    local.get 0
    i64.extend_i32_u
    local.tee 1
    i64.const 17
    i64.add
    i32.load
    i32.const 1
    i32.shr_u
    local.get 1
    i64.const 9
    i64.add
    i64.load32_u
    i64.const 5
    i64.add
    i32.load
    i32.ge_u)
  (func $blob_iter_next (type 7) (param i32) (result i32)
    (local i64 i64)
    local.get 0
    i64.extend_i32_u
    local.tee 1
    i64.const 17
    i64.add
    local.tee 2
    local.get 2
    i32.load
    local.tee 0
    i32.const -2
    i32.and
    i32.const 2
    i32.add
    i32.store
    local.get 1
    i64.const 9
    i64.add
    i64.load32_u
    local.get 0
    i32.const 1
    i32.shr_u
    i64.extend_i32_u
    i64.add
    i64.const 9
    i64.add
    i32.load8_u)
  (func $skip_leb128 (type 11) (param i64)
    (local i64 i64 i32)
    local.get 0
    i64.load align=1
    local.set 1
    local.get 0
    i64.load offset=8 align=1
    local.set 2
    block  ;; label = @1
      loop  ;; label = @2
        local.get 1
        local.get 2
        i64.ge_u
        br_if 1 (;@1;)
        local.get 1
        i32.load8_s
        local.set 3
        local.get 0
        local.get 1
        i64.const 1
        i64.add
        local.tee 1
        i64.store align=1
        local.get 1
        local.set 1
        local.get 3
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
      end
      return
    end
    call $motoko_rts::idl_trap_with::hfa6bb08396dac7db
    unreachable)
  (func $motoko_rts::idl_trap_with::hfa6bb08396dac7db (type 10)
    (local i64)
    global.get $__memory_base
    local.tee 0
    i64.const 1995
    i64.add
    local.get 0
    i64.const 191
    i64.add
    i64.const 23
    call $motoko_rts::trap_with_prefix::hd663c6f70869e877
    unreachable)
  (func $char_to_upper (type 7) (param i32) (result i32)
    (local i64 i64 i64 i64)
    global.get $__stack_pointer
    i64.const 96
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i64.const 48
    i64.add
    local.get 0
    call $core::unicode::unicode_data::conversions::to_upper::h508b12c71146707b
    local.get 1
    i64.const 4
    i64.add
    local.get 1
    i64.const 48
    i64.add
    call $core::char::CaseMappingIter::new::hd01f267f074fcbfc
    local.get 1
    i64.const 48
    i64.add
    local.get 1
    i64.const 4
    i64.add
    call $_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::size_hint::h76de20e1c0d8be2a
    local.get 1
    i64.load offset=48
    local.set 2
    local.get 1
    local.get 1
    i64.const 64
    i64.add
    i64.load
    local.tee 3
    i64.store offset=24
    local.get 1
    local.get 1
    i64.load offset=56
    local.tee 4
    i64.store offset=16
    local.get 1
    i64.const 1
    i64.store offset=32
    local.get 1
    local.get 2
    i64.store offset=40
    block  ;; label = @1
      block  ;; label = @2
        local.get 4
        i64.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 2
        i64.ne
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 2
          i64.const 1
          i64.ne
          br_if 0 (;@3;)
          local.get 1
          i64.const 4
          i64.add
          call $_$LT$core..char..ToUppercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::next::h7c74682c8f2bd818
          local.tee 0
          i32.const 1114112
          i32.eq
          br_if 2 (;@1;)
        end
        local.get 1
        i64.const 96
        i64.add
        global.set $__stack_pointer
        local.get 0
        return
      end
      local.get 1
      i64.const 0
      i64.store offset=64
      i32.const 0
      local.get 1
      i64.const 16
      i64.add
      local.get 1
      i64.const 32
      i64.add
      local.get 1
      i64.const 48
      i64.add
      global.get $__memory_base
      i64.const 3016
      i64.add
      call $core::panicking::assert_failed::h418c82b05c5b11d1
      unreachable
    end
    global.get $__memory_base
    local.tee 1
    i64.const 148
    i64.add
    i64.const 43
    local.get 1
    i64.const 3104
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $char_to_lower (type 7) (param i32) (result i32)
    (local i64 i64 i64 i64)
    global.get $__stack_pointer
    i64.const 96
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i64.const 48
    i64.add
    local.get 0
    call $core::unicode::unicode_data::conversions::to_lower::ha7938dd9ffb4d0ad
    local.get 1
    i64.const 4
    i64.add
    local.get 1
    i64.const 48
    i64.add
    call $core::char::CaseMappingIter::new::hd01f267f074fcbfc
    local.get 1
    i64.const 48
    i64.add
    local.get 1
    i64.const 4
    i64.add
    call $_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::size_hint::he2722b4f780a6b65
    local.get 1
    i64.load offset=48
    local.set 2
    local.get 1
    local.get 1
    i64.const 64
    i64.add
    i64.load
    local.tee 3
    i64.store offset=24
    local.get 1
    local.get 1
    i64.load offset=56
    local.tee 4
    i64.store offset=16
    local.get 1
    i64.const 1
    i64.store offset=32
    local.get 1
    local.get 2
    i64.store offset=40
    block  ;; label = @1
      block  ;; label = @2
        local.get 4
        i64.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 2
        i64.ne
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 2
          i64.const 1
          i64.ne
          br_if 0 (;@3;)
          local.get 1
          i64.const 4
          i64.add
          call $_$LT$core..char..ToLowercase$u20$as$u20$core..iter..traits..iterator..Iterator$GT$::next::h5a9ec19c206cf828
          local.tee 0
          i32.const 1114112
          i32.eq
          br_if 2 (;@1;)
        end
        local.get 1
        i64.const 96
        i64.add
        global.set $__stack_pointer
        local.get 0
        return
      end
      local.get 1
      i64.const 0
      i64.store offset=64
      i32.const 0
      local.get 1
      i64.const 16
      i64.add
      local.get 1
      i64.const 32
      i64.add
      local.get 1
      i64.const 48
      i64.add
      global.get $__memory_base
      i64.const 3016
      i64.add
      call $core::panicking::assert_failed::h418c82b05c5b11d1
      unreachable
    end
    global.get $__memory_base
    local.tee 1
    i64.const 148
    i64.add
    i64.const 43
    local.get 1
    i64.const 3128
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $char_is_whitespace (type 7) (param i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const -9
        i32.add
        local.tee 1
        i32.const 24
        i32.lt_u
        br_if 0 (;@2;)
        i32.const 0
        local.set 1
        local.get 0
        i32.const 128
        i32.lt_u
        br_if 1 (;@1;)
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 8
            i32.shr_u
            local.tee 2
            i32.const 31
            i32.gt_s
            br_if 0 (;@4;)
            local.get 2
            i32.eqz
            br_if 1 (;@3;)
            local.get 2
            i32.const 22
            i32.ne
            br_if 3 (;@1;)
            local.get 0
            i32.const 5760
            i32.eq
            return
          end
          block  ;; label = @4
            local.get 2
            i32.const 32
            i32.eq
            br_if 0 (;@4;)
            local.get 2
            i32.const 48
            i32.ne
            br_if 3 (;@1;)
            local.get 0
            i32.const 12288
            i32.eq
            return
          end
          global.get $core::unicode::unicode_data::white_space::WHITESPACE_MAP::ha9af54a48f3239a7
          local.get 0
          i64.extend_i32_u
          i64.const 255
          i64.and
          i64.add
          i32.load8_u
          i32.const 2
          i32.and
          i32.const 1
          i32.shr_u
          return
        end
        global.get $core::unicode::unicode_data::white_space::WHITESPACE_MAP::ha9af54a48f3239a7
        local.get 0
        i64.extend_i32_u
        i64.const 255
        i64.and
        i64.add
        i32.load8_u
        i32.const 1
        i32.and
        return
      end
      i32.const 8388639
      local.get 1
      i32.shr_u
      i32.const 1
      i32.and
      local.set 1
    end
    local.get 1)
  (func $char_is_uppercase (type 7) (param i32) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const -65
      i32.add
      i32.const 26
      i32.lt_u
      br_if 0 (;@1;)
      i32.const 0
      local.set 1
      local.get 0
      i32.const 128
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      call $core::unicode::unicode_data::uppercase::lookup::h86d90efac1b8d8c5
      local.set 1
    end
    local.get 1)
  (func $char_is_lowercase (type 7) (param i32) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const -97
      i32.add
      i32.const 26
      i32.lt_u
      br_if 0 (;@1;)
      i32.const 0
      local.set 1
      local.get 0
      i32.const 128
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      call $core::unicode::unicode_data::lowercase::lookup::he4c936c8b6cc6e22
      local.set 1
    end
    local.get 1)
  (func $char_is_alphabetic (type 7) (param i32) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const 2097119
      i32.and
      i32.const -65
      i32.add
      i32.const 26
      i32.lt_u
      br_if 0 (;@1;)
      i32.const 0
      local.set 1
      local.get 0
      i32.const 128
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      call $core::unicode::unicode_data::alphabetic::lookup::hf08775a06e39ded4
      local.set 1
    end
    local.get 1)
  (func $peek_future_continuation (type 7) (param i32) (result i32)
    (local i32 i64)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          global.get $__memory_base
          i64.const 3784
          i64.add
          i32.load
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i64.extend_i32_u
          local.tee 2
          i64.const 5
          i64.add
          i32.load
          local.get 0
          i32.le_u
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 2
            i64.const 1
            i64.add
            local.get 0
            i32.const 3
            i32.shl
            i64.extend_i32_u
            i64.add
            i32.load offset=8
            local.tee 0
            i32.const 1
            i32.eq
            br_if 0 (;@4;)
            local.get 0
            i32.const 1
            i32.and
            br_if 3 (;@1;)
          end
          global.get $__memory_base
          i64.const 340
          i64.add
          i64.const 57
          call $motoko_rts::rts_trap_with::h000639acaf03dda1
          unreachable
        end
        global.get $__memory_base
        i64.const 225
        i64.add
        i64.const 58
        call $motoko_rts::rts_trap_with::h000639acaf03dda1
        unreachable
      end
      global.get $__memory_base
      i64.const 283
      i64.add
      i64.const 57
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    local.get 0
    i64.extend_i32_u
    i64.const 25
    i64.add
    i32.load)
  (func $motoko_rts::rts_trap_with::h000639acaf03dda1 (type 3) (param i64 i64)
    global.get $__memory_base
    i64.const 2006
    i64.add
    local.get 0
    local.get 1
    call $motoko_rts::trap_with_prefix::hd663c6f70869e877
    unreachable)
  (func $recall_continuation (type 7) (param i32) (result i32)
    (local i32 i64 i64 i64)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          global.get $__memory_base
          i64.const 3784
          i64.add
          i32.load
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i64.extend_i32_u
          local.tee 2
          i64.const 5
          i64.add
          i32.load
          local.get 0
          i32.le_u
          br_if 1 (;@2;)
          local.get 2
          i64.const 1
          i64.add
          local.get 0
          i32.const 3
          i32.shl
          i64.extend_i32_u
          i64.add
          local.tee 2
          i32.load offset=8
          local.set 1
          local.get 2
          global.get $__memory_base
          local.tee 3
          i64.const 3792
          i64.add
          local.tee 4
          i32.load
          i32.const 1
          i32.shl
          i32.store offset=8
          local.get 4
          local.get 0
          i32.store
          local.get 3
          i64.const 3788
          i64.add
          local.tee 2
          local.get 2
          i32.load
          i32.const -1
          i32.add
          i32.store
          block  ;; label = @4
            local.get 1
            i32.const 1
            i32.eq
            br_if 0 (;@4;)
            local.get 1
            i32.const 1
            i32.and
            br_if 3 (;@1;)
          end
          global.get $__memory_base
          i64.const 502
          i64.add
          i64.const 52
          call $motoko_rts::rts_trap_with::h000639acaf03dda1
          unreachable
        end
        global.get $__memory_base
        i64.const 397
        i64.add
        i64.const 53
        call $motoko_rts::rts_trap_with::h000639acaf03dda1
        unreachable
      end
      global.get $__memory_base
      i64.const 450
      i64.add
      i64.const 52
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    local.get 1)
  (func $continuation_count (type 9) (result i32)
    global.get $__memory_base
    i64.const 3788
    i64.add
    i32.load)
  (func $continuation_table_size (type 9) (result i32)
    (local i32)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3784
      i64.add
      i32.load
      local.tee 0
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    local.get 0
    i64.extend_i32_u
    i64.const 5
    i64.add
    i32.load)
  (func $leb128_encode (type 13) (param i32 i64)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 127
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.set 2
        br 1 (;@1;)
      end
      loop  ;; label = @2
        local.get 1
        local.get 0
        i32.const 128
        i32.or
        i32.store8
        local.get 1
        i64.const 1
        i64.add
        local.set 1
        local.get 0
        i32.const 16384
        i32.lt_u
        local.set 3
        local.get 0
        i32.const 7
        i32.shr_u
        local.tee 2
        local.set 0
        local.get 3
        i32.eqz
        br_if 0 (;@2;)
      end
    end
    local.get 1
    local.get 2
    i32.const 127
    i32.and
    i32.store8)
  (func $sleb128_encode (type 13) (param i32 i64)
    (local i32 i32 i32)
    local.get 0
    i32.const 64
    i32.and
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 127
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.set 3
        local.get 2
        i32.eqz
        br_if 1 (;@1;)
      end
      loop  ;; label = @2
        local.get 0
        i32.const 7
        i32.shr_s
        local.set 3
        block  ;; label = @3
          local.get 2
          i32.const 255
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 3
          i32.const -1
          i32.ne
          br_if 0 (;@3;)
          local.get 0
          local.set 3
          br 2 (;@1;)
        end
        local.get 1
        local.get 0
        i32.const 128
        i32.or
        i32.store8
        local.get 3
        i32.const 64
        i32.and
        local.set 2
        local.get 1
        i64.const 1
        i64.add
        local.set 1
        local.get 0
        i32.const 16383
        i32.gt_u
        local.set 4
        local.get 3
        local.set 0
        local.get 4
        br_if 0 (;@2;)
        local.get 3
        local.set 0
        local.get 2
        br_if 0 (;@2;)
      end
    end
    local.get 1
    local.get 3
    i32.const 127
    i32.and
    i32.store8)
  (func $leb128_decode (type 4) (param i64) (result i32)
    (local i64 i64 i32 i32 i32)
    local.get 0
    i64.load align=1
    local.set 1
    local.get 0
    i64.load offset=8 align=1
    local.set 2
    i32.const 0
    local.set 3
    i32.const 0
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 1
          local.get 2
          i64.ge_u
          br_if 1 (;@2;)
          local.get 1
          i32.load8_u
          local.set 5
          local.get 0
          local.get 1
          i64.const 1
          i64.add
          local.tee 1
          i64.store align=1
          block  ;; label = @4
            local.get 3
            i32.const 28
            i32.ne
            br_if 0 (;@4;)
            local.get 5
            i32.const 255
            i32.and
            i32.const 15
            i32.gt_u
            br_if 3 (;@1;)
          end
          local.get 5
          i32.const 127
          i32.and
          local.get 3
          i32.shl
          local.get 4
          i32.or
          local.set 4
          local.get 1
          local.set 1
          local.get 3
          i32.const 7
          i32.add
          local.set 3
          local.get 5
          i32.extend8_s
          i32.const -1
          i32.le_s
          br_if 0 (;@3;)
        end
        local.get 4
        return
      end
      call $motoko_rts::idl_trap_with::hfa6bb08396dac7db
      unreachable
    end
    global.get $__memory_base
    local.tee 1
    i64.const 554
    i64.add
    i64.const 23
    local.get 1
    i64.const 3152
    i64.add
    call $core::option::expect_failed::h71d0f13fa0de0cdb
    unreachable)
  (func $sleb128_decode (type 4) (param i64) (result i32)
    (local i64 i64 i32 i32 i32 i32 i32)
    local.get 0
    i64.load align=1
    local.set 1
    local.get 0
    i64.load offset=8 align=1
    local.set 2
    i32.const 0
    local.set 3
    i32.const 0
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 1
          local.tee 1
          local.get 2
          i64.ge_u
          br_if 1 (;@2;)
          local.get 1
          i32.load8_u
          local.set 5
          local.get 0
          local.get 1
          i64.const 1
          i64.add
          local.tee 1
          i64.store align=1
          local.get 5
          i32.extend8_s
          local.set 6
          block  ;; label = @4
            local.get 3
            i32.const 28
            i32.ne
            br_if 0 (;@4;)
            local.get 6
            i32.const 120
            i32.and
            local.tee 7
            i32.const 120
            i32.eq
            br_if 0 (;@4;)
            local.get 7
            br_if 3 (;@1;)
          end
          local.get 5
          i32.const 127
          i32.and
          local.get 3
          i32.shl
          local.get 4
          i32.or
          local.set 4
          local.get 3
          i32.const 7
          i32.add
          local.set 3
          local.get 6
          i32.const -1
          i32.le_s
          br_if 0 (;@3;)
        end
        local.get 4
        i32.const -1
        local.get 3
        i32.shl
        i32.const 0
        local.get 6
        i32.const 64
        i32.and
        i32.const 6
        i32.shr_u
        select
        i32.const 0
        local.get 3
        i32.const 32
        i32.lt_s
        select
        i32.or
        return
      end
      call $motoko_rts::idl_trap_with::hfa6bb08396dac7db
      unreachable
    end
    global.get $__memory_base
    local.tee 1
    i64.const 590
    i64.add
    i64.const 24
    local.get 1
    i64.const 3176
    i64.add
    call $core::option::expect_failed::h71d0f13fa0de0cdb
    unreachable)
  (func $get_max_live_size (type 9) (result i32)
    global.get $__memory_base
    i64.const 3796
    i64.add
    i32.load)
  (func $compute_crc32 (type 7) (param i32) (result i32)
    (local i64 i64 i64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.extend_i32_u
        local.tee 1
        i32.load offset=1
        i32.const 17
        i32.ne
        br_if 0 (;@2;)
        local.get 1
        i64.const 1
        i64.add
        local.tee 1
        i32.load offset=4
        local.tee 0
        br_if 1 (;@1;)
        i32.const 0
        return
      end
      global.get $__memory_base
      local.tee 1
      i64.const 632
      i64.add
      i64.const 28
      local.get 1
      i64.const 3200
      i64.add
      call $core::panicking::panic::h1108c90903a88b24
      unreachable
    end
    local.get 1
    i64.const 8
    i64.add
    local.set 2
    local.get 0
    i64.extend_i32_u
    local.set 3
    i32.const -1
    local.set 0
    i64.const 0
    local.set 1
    loop  ;; label = @1
      global.get $__memory_base
      i64.const 680
      i64.add
      local.get 2
      local.get 1
      i64.add
      i32.load8_u
      local.get 0
      i32.xor
      i64.extend_i32_u
      i64.const 255
      i64.and
      i64.const 2
      i64.shl
      i64.add
      i32.load
      local.get 0
      i32.const 8
      i32.shr_u
      i32.xor
      local.set 0
      local.get 1
      i64.const 1
      i64.add
      local.tee 1
      local.get 3
      i64.ne
      br_if 0 (;@1;)
    end
    local.get 0
    i32.const -1
    i32.xor)
  (func $motoko_rts::principal_id::enc_stash::h9337051efa825169 (type 2) (param i64 i32)
    (local i32 i32 i64)
    local.get 0
    local.get 0
    i32.load offset=20
    local.get 0
    i32.load offset=8
    local.tee 2
    i32.add
    local.tee 3
    i32.store offset=20
    local.get 0
    local.get 0
    i32.load offset=16
    local.get 2
    i32.shl
    local.get 1
    i32.const 255
    i32.and
    i32.or
    local.tee 1
    i32.store offset=16
    block  ;; label = @1
      local.get 3
      local.get 0
      i32.load offset=12
      local.tee 2
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i64.load
      local.set 4
      loop  ;; label = @2
        local.get 4
        global.get $__memory_base
        i64.const 1704
        i64.add
        local.get 1
        local.get 3
        local.get 2
        i32.sub
        local.tee 3
        i32.shr_u
        i64.extend_i32_u
        i64.const 31
        i64.and
        i64.add
        i32.load8_u
        i32.store8
        local.get 4
        i64.const 1
        i64.add
        local.set 4
        local.get 1
        i32.const -1
        local.get 3
        i32.shl
        i32.const -1
        i32.xor
        i32.and
        local.set 1
        local.get 3
        local.get 2
        i32.ge_u
        br_if 0 (;@2;)
      end
      local.get 0
      local.get 1
      i32.store offset=16
      local.get 0
      local.get 4
      i64.store
      local.get 0
      local.get 3
      i32.store offset=20
    end)
  (func $motoko_rts::text::alloc_text_blob::h11ccee3a71497588 (type 7) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      i32.const 1073741823
      i32.gt_u
      br_if 0 (;@1;)
      local.get 0
      call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
      return
    end
    global.get $__memory_base
    i64.const 1776
    i64.add
    i64.const 31
    call $motoko_rts::rts_trap_with::h000639acaf03dda1
    unreachable)
  (func $motoko_rts::memory::alloc_blob::h5034ff391397ca60 (type 7) (param i32) (result i32)
    (local i32 i64)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3928
      i64.add
      i32.load
      local.tee 1
      i64.extend_i32_u
      local.get 0
      i32.const 7
      i32.add
      i32.const -8
      i32.and
      i32.const 8
      i32.add
      i64.extend_i32_u
      i64.add
      local.tee 2
      memory.size
      i64.const 16
      i64.shl
      i64.le_u
      br_if 0 (;@1;)
      local.get 2
      call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
    end
    global.get $__memory_base
    i64.const 3928
    i64.add
    local.get 2
    i64.store32
    local.get 1
    i32.const -1
    i32.add
    local.tee 1
    i64.extend_i32_u
    local.tee 2
    i32.const 17
    i32.store offset=1
    local.get 2
    i64.const 5
    i64.add
    local.get 0
    i32.store
    local.get 1)
  (func $text_to_buf (type 13) (param i32 i64)
    (local i64 i64 i64 i64)
    i64.const 0
    local.set 2
    block  ;; label = @1
      loop  ;; label = @2
        local.get 0
        i64.extend_i32_u
        local.tee 3
        i64.const 1
        i64.add
        local.set 4
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.load offset=1
            i32.const 17
            i32.eq
            br_if 0 (;@4;)
            loop  ;; label = @5
              local.get 4
              i64.load32_u offset=8
              local.tee 5
              i64.const 1
              i64.add
              local.set 3
              local.get 1
              local.get 5
              i64.const 5
              i64.add
              i64.load32_u
              i64.add
              local.set 5
              block  ;; label = @6
                local.get 4
                i32.load offset=12
                local.tee 0
                i64.extend_i32_u
                i64.const 5
                i64.add
                i32.load
                i32.const 12
                i32.lt_u
                br_if 0 (;@6;)
                local.get 5
                local.get 2
                i64.store offset=4 align=1
                local.get 5
                local.get 0
                i32.store align=1
                local.get 3
                local.set 4
                local.get 5
                local.set 2
                local.get 3
                i32.load
                i32.const 17
                i32.ne
                br_if 1 (;@5;)
                local.get 1
                local.get 3
                i64.const 8
                i64.add
                local.get 3
                i64.load32_u offset=4
                memory.copy
                local.get 5
                local.set 2
                br 3 (;@3;)
              end
              local.get 0
              local.get 5
              call $text_to_buf
              local.get 3
              local.set 4
              local.get 3
              i32.load
              i32.const 17
              i32.ne
              br_if 0 (;@5;)
            end
            local.get 3
            local.set 4
          end
          local.get 1
          local.get 4
          i64.const 8
          i64.add
          local.get 4
          i64.load32_u offset=4
          memory.copy
          local.get 2
          i64.eqz
          br_if 2 (;@1;)
        end
        local.get 2
        local.tee 1
        i64.load offset=4 align=1
        local.set 2
        local.get 1
        i32.load align=1
        local.set 0
        br 0 (;@2;)
      end
    end)
  (func $text_size (type 7) (param i32) (result i32)
    local.get 0
    i64.extend_i32_u
    i64.const 5
    i64.add
    i32.load)
  (func $text_compare (type 14) (param i32 i32) (result i32)
    (local i64)
    global.get $__memory_base
    local.tee 2
    i64.const 1807
    i64.add
    i64.const 15
    local.get 2
    i64.const 3224
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $text_len (type 7) (param i32) (result i32)
    (local i64 i64)
    local.get 0
    i64.extend_i32_u
    local.tee 1
    i64.const 1
    i64.add
    local.set 2
    i32.const 0
    local.set 0
    block  ;; label = @1
      local.get 1
      i32.load offset=1
      i32.const 17
      i32.eq
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 2
        i32.load offset=8
        call $text_len
        local.get 0
        i32.add
        local.set 0
        local.get 2
        i64.load32_u offset=12
        local.tee 1
        i64.const 1
        i64.add
        local.set 2
        local.get 1
        i32.load offset=1
        i32.const 17
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 2
    i64.const 8
    i64.add
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i64.load32_u offset=4
        local.tee 2
        i64.const 32
        i64.lt_u
        br_if 0 (;@2;)
        local.get 1
        local.get 2
        call $core::str::count::do_count_chars::h0aeec386532c2f2b
        local.set 2
        br 1 (;@1;)
      end
      local.get 1
      local.get 2
      call $core::str::count::char_count_general_case::h9ee082f639101326
      local.set 2
    end
    local.get 0
    local.get 2
    i32.wrap_i64
    i32.add)
  (func $motoko_rts::text_iter::find_leaf::h5cbf944c560302e0 (type 15) (param i32 i64) (result i32)
    (local i64 i64 i64)
    block  ;; label = @1
      local.get 0
      i64.extend_i32_u
      local.tee 2
      i32.load offset=1
      i32.const 25
      i32.ne
      br_if 0 (;@1;)
      local.get 2
      i64.const 1
      i64.add
      local.set 2
      loop  ;; label = @2
        i32.const 2
        call $motoko_rts::memory::alloc_array::h2a91ff2be1084c58
        local.tee 0
        i64.extend_i32_u
        local.tee 3
        i64.const 9
        i64.add
        local.tee 4
        local.get 2
        i32.load offset=12
        i32.store
        local.get 4
        i32.wrap_i64
        call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
        local.get 3
        i64.const 17
        i64.add
        local.tee 3
        local.get 1
        i32.load
        i32.store
        local.get 3
        i32.wrap_i64
        call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
        local.get 1
        local.get 0
        i32.store
        local.get 2
        i32.load offset=8
        local.tee 0
        i64.extend_i32_u
        local.tee 3
        i64.const 1
        i64.add
        local.set 2
        local.get 3
        i32.load offset=1
        i32.const 25
        i32.eq
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func $motoko_rts::memory::alloc_array::h2a91ff2be1084c58 (type 7) (param i32) (result i32)
    (local i32 i64)
    block  ;; label = @1
      local.get 0
      i32.const 536870912
      i32.gt_u
      br_if 0 (;@1;)
      block  ;; label = @2
        global.get $__memory_base
        i64.const 3928
        i64.add
        i32.load
        local.tee 1
        i64.extend_i32_u
        local.get 0
        i32.const 3
        i32.shl
        i32.const 8
        i32.add
        i64.extend_i32_u
        i64.add
        local.tee 2
        memory.size
        i64.const 16
        i64.shl
        i64.le_u
        br_if 0 (;@2;)
        local.get 2
        call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
      end
      global.get $__memory_base
      i64.const 3928
      i64.add
      local.get 2
      i64.store32
      local.get 1
      i32.const -1
      i32.add
      local.tee 1
      i64.extend_i32_u
      local.tee 2
      i32.const 5
      i32.store offset=1
      local.get 2
      i64.const 5
      i64.add
      local.get 0
      i32.store
      local.get 1
      return
    end
    global.get $__memory_base
    i64.const 2886
    i64.add
    i64.const 26
    call $motoko_rts::rts_trap_with::h000639acaf03dda1
    unreachable)
  (func $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334 (type 16) (param i32)
    (local i32 i32 i64)
    block  ;; label = @1
      block  ;; label = @2
        global.get $__memory_base
        i64.const 3860
        i64.add
        i32.load
        local.tee 1
        local.get 0
        i32.le_u
        br_if 0 (;@2;)
        local.get 0
        i64.extend_i32_u
        i32.load
        local.set 2
        global.get $__memory_base
        local.set 3
        local.get 2
        i32.const 1
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i64.extend_i32_u
        i64.const 1
        i64.add
        local.get 1
        i64.extend_i32_u
        i64.lt_u
        br_if 0 (;@2;)
        local.get 3
        i64.const 3856
        i64.add
        i32.load
        local.get 0
        i32.gt_u
        br_if 0 (;@2;)
        global.get $__memory_base
        i64.const 3832
        i64.add
        i64.load
        i64.const 0
        i64.eq
        br_if 1 (;@1;)
        local.get 0
        call $motoko_rts::gc::generational::remembered_set::RememberedSet::insert::h13cb425f8573411a
      end
      return
    end
    global.get $__memory_base
    local.tee 3
    i64.const 148
    i64.add
    i64.const 43
    local.get 3
    i64.const 3760
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $text_iter_done (type 7) (param i32) (result i32)
    i32.const 0)
  (func $motoko_rts::types::Blob::shrink::hf021b0a99a21983f (type 2) (param i64 i32)
    (local i32 i32 i32 i64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=4
        i32.const 7
        i32.add
        i32.const 3
        i32.shr_u
        local.tee 2
        local.get 1
        i32.const 7
        i32.add
        i32.const 3
        i32.shr_u
        local.tee 3
        i32.sub
        local.tee 4
        i32.const 1
        i32.eq
        br_if 0 (;@2;)
        local.get 2
        local.get 3
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        local.get 3
        i64.extend_i32_u
        i64.const 2
        i64.shl
        i64.add
        local.tee 5
        i64.const 12
        i64.add
        local.get 4
        i32.const -1
        i32.add
        i32.store
        local.get 5
        i64.const 8
        i64.add
        i32.const 31
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 3
      i64.extend_i32_u
      i64.const 2
      i64.shl
      i64.add
      i64.const 8
      i64.add
      i32.const 29
      i32.store
    end
    local.get 0
    local.get 1
    i32.store offset=4)
  (func $motoko_rts::types::block_size::h0bf0d784741e3e30 (type 4) (param i64) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 0
                      i32.load
                      i32.const -1
                      i32.add
                      br_table 1 (;@8;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 2 (;@7;) 0 (;@9;) 6 (;@3;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 3 (;@6;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 6 (;@3;) 0 (;@9;) 4 (;@5;) 0 (;@9;) 5 (;@4;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 0 (;@9;) 0 (;@9;) 6 (;@3;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 8 (;@1;) 0 (;@9;) 7 (;@2;) 0 (;@9;)
                    end
                    global.get $__memory_base
                    i64.const 1840
                    i64.add
                    i64.const 31
                    call $motoko_rts::rts_trap_with::h000639acaf03dda1
                    unreachable
                  end
                  local.get 0
                  i32.load offset=4
                  i32.const 2
                  i32.add
                  return
                end
                local.get 0
                i32.load offset=4
                i32.const 1
                i32.add
                return
              end
              local.get 0
              i32.load offset=8
              i32.const 2
              i32.add
              return
            end
            local.get 0
            i32.load offset=4
            i32.const 7
            i32.add
            i32.const 3
            i32.shr_u
            i32.const 1
            i32.add
            return
          end
          global.get $__memory_base
          i64.const 1871
          i64.add
          i64.const 31
          call $motoko_rts::rts_trap_with::h000639acaf03dda1
          unreachable
        end
        i32.const 2
        return
      end
      local.get 0
      i32.load offset=4
      i32.const 1
      i32.add
      local.set 1
    end
    local.get 1)
  (func $motoko_rts::trap_with_prefix::hd663c6f70869e877 (type 6) (param i64 i64 i64)
    (local i64 i64 i64)
    global.get $__stack_pointer
    i64.const 544
    i64.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i64.const 32
    i64.add
    i32.const 0
    i64.const 512
    memory.fill
    local.get 3
    i64.const 16
    i64.add
    local.get 0
    i64.const 11
    call $core::slice::iter::_$LT$impl$u20$core..iter..traits..collect..IntoIterator$u20$for$u20$$RF$$u5b$T$u5d$$GT$::into_iter::h255c1c79ea70b7e8
    i64.const 0
    local.set 0
    block  ;; label = @1
      local.get 3
      i64.load offset=24
      local.tee 4
      local.get 3
      i64.load offset=16
      local.tee 5
      i64.eq
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 3
        i64.const 32
        i64.add
        local.get 0
        i64.add
        local.get 4
        i32.load8_u
        i32.store8
        local.get 0
        i64.const 1
        i64.add
        local.set 0
        local.get 4
        i64.const 1
        i64.add
        local.tee 4
        local.get 5
        i64.eq
        br_if 1 (;@1;)
        local.get 0
        i64.const 512
        i64.ne
        br_if 0 (;@2;)
      end
    end
    local.get 3
    local.get 1
    local.get 2
    call $core::slice::iter::_$LT$impl$u20$core..iter..traits..collect..IntoIterator$u20$for$u20$$RF$$u5b$T$u5d$$GT$::into_iter::h255c1c79ea70b7e8
    block  ;; label = @1
      local.get 3
      i64.load offset=8
      local.tee 4
      local.get 3
      i64.load
      local.tee 5
      i64.eq
      br_if 0 (;@1;)
      local.get 0
      i64.const 512
      i64.eq
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 3
        i64.const 32
        i64.add
        local.get 0
        i64.add
        local.get 4
        i32.load8_u
        i32.store8
        local.get 0
        i64.const 1
        i64.add
        local.set 0
        local.get 4
        i64.const 1
        i64.add
        local.tee 4
        local.get 5
        i64.eq
        br_if 1 (;@1;)
        local.get 0
        i64.const 512
        i64.ne
        br_if 0 (;@2;)
      end
    end
    local.get 3
    i64.const 32
    i64.add
    local.get 0
    i32.wrap_i64
    call $rts_trap
    unreachable)
  (func $blob_iter (type 7) (param i32) (result i32)
    (local i64 i32 i64)
    memory.size
    local.set 1
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3928
      i64.add
      i32.load
      local.tee 2
      i64.extend_i32_u
      i64.const 24
      i64.add
      local.tee 3
      local.get 1
      i64.const 16
      i64.shl
      i64.le_u
      br_if 0 (;@1;)
      local.get 3
      call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
    end
    global.get $__memory_base
    i64.const 3928
    i64.add
    local.get 3
    i64.store32
    local.get 2
    i32.const -1
    i32.add
    local.tee 2
    i64.extend_i32_u
    local.tee 3
    i64.const 8589934597
    i64.store offset=1 align=4
    local.get 3
    i64.const 9
    i64.add
    local.tee 1
    local.get 0
    i32.store
    local.get 1
    i32.wrap_i64
    call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
    local.get 3
    i64.const 17
    i64.add
    i32.const 0
    i32.store
    local.get 2)
  (func $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f (type 11) (param i64)
    (local i64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.const 4294901761
        i64.ge_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 0
          i64.const 65535
          i64.add
          i64.const 16
          i64.shr_u
          local.tee 0
          memory.size
          local.tee 1
          i64.le_u
          br_if 0 (;@3;)
          local.get 0
          local.get 1
          i64.sub
          memory.grow
          i64.const -1
          i64.eq
          br_if 2 (;@1;)
        end
        return
      end
      global.get $__memory_base
      i64.const 614
      i64.add
      i64.const 18
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    global.get $__memory_base
    i64.const 614
    i64.add
    i64.const 18
    call $motoko_rts::rts_trap_with::h000639acaf03dda1
    unreachable)
  (func $remember_continuation (type 7) (param i32) (result i32)
    (local i32 i64 i32 i64 i32 i64 i64 i64 i64)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3784
      i64.add
      i32.load
      local.tee 1
      br_if 0 (;@1;)
      global.get $__memory_base
      local.set 2
      i32.const 256
      call $motoko_rts::memory::alloc_array::h2a91ff2be1084c58
      local.set 3
      local.get 2
      i64.const 3792
      i64.add
      i32.const 0
      i32.store
      local.get 2
      i64.const 3784
      i64.add
      local.get 3
      i32.store
      local.get 2
      i64.const 3788
      i64.add
      i32.const 0
      i32.store
      local.get 3
      i64.extend_i32_u
      i64.const 9
      i64.add
      local.set 4
      i64.const 0
      local.set 2
      loop  ;; label = @2
        local.get 2
        i64.const 3
        i64.shl
        local.get 4
        i64.add
        local.get 2
        i32.wrap_i64
        i32.const 1
        i32.shl
        i32.const 2
        i32.add
        i32.store
        local.get 2
        i64.const 1
        i64.add
        local.tee 2
        i64.const 256
        i64.ne
        br_if 0 (;@2;)
      end
      global.get $__memory_base
      i64.const 3784
      i64.add
      i32.load
      local.set 1
    end
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3792
      i64.add
      i32.load
      local.tee 3
      local.get 1
      i64.extend_i32_u
      local.tee 2
      i64.const 5
      i64.add
      i32.load
      i32.ne
      br_if 0 (;@1;)
      global.get $__memory_base
      i64.const 3784
      i64.add
      local.get 3
      i32.const 1
      i32.shl
      local.tee 5
      call $motoko_rts::memory::alloc_array::h2a91ff2be1084c58
      local.tee 1
      i32.store
      local.get 3
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i64.const 1
      i64.add
      i64.const 8
      i64.add
      local.set 6
      local.get 1
      i64.extend_i32_u
      i64.const 9
      i64.add
      local.set 7
      local.get 3
      i64.extend_i32_u
      local.set 2
      i64.const 0
      local.set 4
      loop  ;; label = @2
        local.get 4
        i64.const 3
        i64.shl
        i64.const 4294967288
        i64.and
        local.tee 8
        local.get 7
        i64.add
        local.tee 9
        local.get 8
        local.get 6
        i64.add
        i32.load
        i32.store
        local.get 9
        i32.wrap_i64
        call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
        local.get 4
        i64.const 1
        i64.add
        local.tee 4
        local.get 2
        i64.ne
        br_if 0 (;@2;)
      end
      local.get 3
      i32.const 1
      i32.lt_s
      br_if 0 (;@1;)
      local.get 5
      i64.extend_i32_u
      local.set 4
      loop  ;; label = @2
        local.get 2
        i64.const 3
        i64.shl
        i64.const 4294967288
        i64.and
        local.get 7
        i64.add
        local.get 2
        i32.wrap_i64
        i32.const 1
        i32.shl
        i32.const 2
        i32.add
        i32.store
        local.get 2
        i64.const 1
        i64.add
        local.tee 2
        local.get 4
        i64.ne
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 1
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        i32.const 1
        i32.and
        br_if 1 (;@1;)
      end
      global.get $__memory_base
      i64.const 2017
      i64.add
      i64.const 55
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    global.get $__memory_base
    local.tee 2
    i64.const 3792
    i64.add
    local.tee 4
    local.get 2
    i64.const 3784
    i64.add
    i64.load32_u
    local.get 4
    i32.load
    local.tee 3
    i32.const 3
    i32.shl
    i64.extend_i32_u
    i64.add
    i64.const 9
    i64.add
    local.tee 4
    i32.load
    i32.const 1
    i32.shr_u
    i32.store
    local.get 4
    local.get 0
    i32.store
    local.get 4
    i32.wrap_i64
    call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
    local.get 2
    i64.const 3788
    i64.add
    local.tee 2
    local.get 2
    i32.load
    i32.const 1
    i32.add
    i32.store
    local.get 3)
  (func $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4 (type 6) (param i64 i64 i64)
    (local i64 i64 i32 i32 i64)
    local.get 2
    i64.load32_u
    local.tee 3
    i64.const 1
    i64.add
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        local.get 3
        i32.load offset=1
        i32.const 19
        i32.eq
        br_if 0 (;@2;)
        local.get 4
        call $motoko_rts::types::block_size::h0bf0d784741e3e30
        local.set 5
        block  ;; label = @3
          global.get $__memory_base
          i64.const 3928
          i64.add
          i32.load
          local.tee 6
          i64.extend_i32_u
          local.get 5
          i32.const 3
          i32.shl
          i64.extend_i32_u
          local.tee 7
          i64.add
          local.tee 3
          memory.size
          i64.const 16
          i64.shl
          i64.le_u
          br_if 0 (;@3;)
          local.get 3
          call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
        end
        global.get $__memory_base
        i64.const 3928
        i64.add
        local.get 3
        i64.store32
        local.get 6
        i32.const -1
        i32.add
        i64.extend_i32_u
        i64.const 1
        i64.add
        local.tee 3
        local.get 4
        local.get 7
        memory.copy
        local.get 4
        i32.const 19
        i32.store
        local.get 4
        local.get 0
        local.get 1
        i64.sub
        local.get 3
        i64.add
        i32.wrap_i64
        i32.const -1
        i32.add
        local.tee 5
        i32.store offset=4
        br 1 (;@1;)
      end
      local.get 4
      i32.load offset=4
      local.set 5
    end
    local.get 2
    local.get 5
    i32.store)
  (func $motoko_rts::gc::copying::scav::hcb2e323f2d684a01 (type 17) (param i64 i64 i32)
    (local i64 i64 i64 i64 i32)
    local.get 2
    i64.extend_i32_u
    local.tee 3
    i64.const 1
    i64.add
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 3
                              i32.load offset=1
                              local.tee 2
                              i32.const -1
                              i32.add
                              br_table 0 (;@13;) 9 (;@4;) 2 (;@11;) 9 (;@4;) 10 (;@3;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 7 (;@6;) 9 (;@4;) 6 (;@7;) 9 (;@4;) 5 (;@8;) 9 (;@4;) 4 (;@9;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 3 (;@10;) 9 (;@4;) 1 (;@12;) 9 (;@4;) 11 (;@2;) 9 (;@4;) 11 (;@2;) 9 (;@4;)
                            end
                            local.get 4
                            i32.load offset=4
                            local.tee 2
                            i32.eqz
                            br_if 10 (;@2;)
                            local.get 4
                            i64.const 12
                            i64.add
                            local.set 5
                            local.get 2
                            i64.extend_i32_u
                            local.set 6
                            i64.const 0
                            local.set 3
                            loop  ;; label = @13
                              block  ;; label = @14
                                local.get 5
                                local.get 3
                                i64.const 2
                                i64.shl
                                i64.add
                                local.tee 4
                                i32.load
                                local.tee 2
                                i32.const 1
                                i32.eq
                                br_if 0 (;@14;)
                                local.get 2
                                i32.const 1
                                i32.and
                                i32.eqz
                                br_if 0 (;@14;)
                                local.get 2
                                i64.extend_i32_u
                                i64.const 1
                                i64.add
                                local.get 0
                                i64.lt_u
                                br_if 0 (;@14;)
                                local.get 0
                                local.get 1
                                local.get 4
                                call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
                              end
                              local.get 3
                              i64.const 1
                              i64.add
                              local.tee 3
                              local.get 6
                              i64.ne
                              br_if 0 (;@13;)
                              br 11 (;@2;)
                            end
                          end
                          global.get $__memory_base
                          i64.const 1944
                          i64.add
                          i64.const 51
                          call $motoko_rts::rts_trap_with::h000639acaf03dda1
                          unreachable
                        end
                        local.get 4
                        i32.load offset=4
                        local.tee 2
                        i32.const 1
                        i32.eq
                        br_if 8 (;@2;)
                        local.get 2
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 8 (;@2;)
                        local.get 2
                        i64.extend_i32_u
                        i64.const 1
                        i64.add
                        local.get 0
                        i64.lt_u
                        br_if 8 (;@2;)
                        local.get 4
                        i64.const 4
                        i64.add
                        local.set 3
                        br 5 (;@5;)
                      end
                      block  ;; label = @10
                        local.get 4
                        i32.load offset=8
                        local.tee 2
                        i32.const 1
                        i32.eq
                        br_if 0 (;@10;)
                        local.get 2
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 0 (;@10;)
                        local.get 2
                        i64.extend_i32_u
                        i64.const 1
                        i64.add
                        local.get 0
                        i64.lt_u
                        br_if 0 (;@10;)
                        local.get 0
                        local.get 1
                        local.get 4
                        i64.const 8
                        i64.add
                        call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
                      end
                      local.get 4
                      i32.load offset=12
                      local.tee 2
                      i32.const 1
                      i32.eq
                      br_if 7 (;@2;)
                      local.get 2
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 7 (;@2;)
                      local.get 2
                      i64.extend_i32_u
                      i64.const 1
                      i64.add
                      local.get 0
                      i64.lt_u
                      br_if 7 (;@2;)
                      local.get 4
                      i64.const 12
                      i64.add
                      local.set 3
                      br 4 (;@5;)
                    end
                    local.get 4
                    i32.load offset=8
                    local.tee 2
                    i32.const 1
                    i32.eq
                    br_if 6 (;@2;)
                    local.get 2
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 6 (;@2;)
                    local.get 2
                    i64.extend_i32_u
                    i64.const 1
                    i64.add
                    local.get 0
                    i64.lt_u
                    br_if 6 (;@2;)
                    local.get 4
                    i64.const 8
                    i64.add
                    local.set 3
                    br 3 (;@5;)
                  end
                  local.get 4
                  i32.load offset=4
                  local.tee 2
                  i32.const 1
                  i32.eq
                  br_if 5 (;@2;)
                  local.get 2
                  i32.const 1
                  i32.and
                  i32.eqz
                  br_if 5 (;@2;)
                  local.get 2
                  i64.extend_i32_u
                  i64.const 1
                  i64.add
                  local.get 0
                  i64.lt_u
                  br_if 5 (;@2;)
                  local.get 4
                  i64.const 4
                  i64.add
                  local.set 3
                  br 2 (;@5;)
                end
                local.get 4
                i32.load offset=8
                local.tee 2
                i32.eqz
                br_if 4 (;@2;)
                local.get 4
                i64.const 12
                i64.add
                local.set 5
                local.get 2
                i64.extend_i32_u
                local.set 6
                i64.const 0
                local.set 3
                loop  ;; label = @7
                  block  ;; label = @8
                    local.get 5
                    local.get 3
                    i64.const 2
                    i64.shl
                    i64.add
                    local.tee 4
                    i32.load
                    local.tee 2
                    i32.const 1
                    i32.eq
                    br_if 0 (;@8;)
                    local.get 2
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 2
                    i64.extend_i32_u
                    i64.const 1
                    i64.add
                    local.get 0
                    i64.lt_u
                    br_if 0 (;@8;)
                    local.get 0
                    local.get 1
                    local.get 4
                    call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
                  end
                  local.get 3
                  i64.const 1
                  i64.add
                  local.tee 3
                  local.get 6
                  i64.ne
                  br_if 0 (;@7;)
                  br 5 (;@2;)
                end
              end
              local.get 4
              i32.load offset=4
              local.tee 2
              i32.const 1
              i32.eq
              br_if 3 (;@2;)
              local.get 2
              i32.const 1
              i32.and
              i32.eqz
              br_if 3 (;@2;)
              local.get 2
              i64.extend_i32_u
              i64.const 1
              i64.add
              local.get 0
              i64.lt_u
              br_if 3 (;@2;)
              local.get 4
              i64.const 4
              i64.add
              local.set 3
            end
            local.get 0
            local.get 1
            local.get 3
            call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
            return
          end
          local.get 2
          i32.const 31
          i32.le_u
          br_if 2 (;@1;)
        end
        local.get 2
        i32.const 0
        local.get 2
        i32.const 31
        i32.gt_u
        select
        local.tee 2
        local.get 4
        i32.load offset=4
        local.tee 7
        i32.ge_u
        br_if 0 (;@2;)
        local.get 4
        i64.const 8
        i64.add
        local.set 5
        local.get 2
        i64.extend_i32_u
        local.set 3
        loop  ;; label = @3
          block  ;; label = @4
            local.get 5
            local.get 3
            i64.const 2
            i64.shl
            i64.add
            local.tee 4
            i32.load
            local.tee 2
            i32.const 1
            i32.eq
            br_if 0 (;@4;)
            local.get 2
            i32.const 1
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i64.extend_i32_u
            i64.const 1
            i64.add
            local.get 0
            i64.lt_u
            br_if 0 (;@4;)
            local.get 0
            local.get 1
            local.get 4
            call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
          end
          local.get 7
          local.get 3
          i64.const 1
          i64.add
          local.tee 3
          i32.wrap_i64
          i32.ne
          br_if 0 (;@3;)
        end
      end
      return
    end
    global.get $__memory_base
    i64.const 1902
    i64.add
    i64.const 42
    call $motoko_rts::rts_trap_with::h000639acaf03dda1
    unreachable)
  (func $motoko_rts::gc::generational::mark_stack::push_mark_stack::h75ccea4977ad5e46 (type 11) (param i64)
    (local i64 i64 i64 i32 i32)
    global.get $__stack_pointer
    i64.const 64
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        global.get $__memory_base
        local.tee 2
        i64.const 3824
        i64.add
        i64.load
        local.tee 3
        local.get 2
        i64.const 3816
        i64.add
        i64.load
        i64.ne
        br_if 0 (;@2;)
        block  ;; label = @3
          global.get $__memory_base
          local.tee 2
          i64.const 3928
          i64.add
          i32.load
          local.tee 4
          i64.extend_i32_u
          local.get 2
          i64.const 3800
          i64.add
          i64.load
          i32.load offset=4
          i32.const 7
          i32.add
          local.tee 5
          i32.const -8
          i32.and
          i64.extend_i32_u
          i64.add
          local.tee 2
          memory.size
          i64.const 16
          i64.shl
          i64.le_u
          br_if 0 (;@3;)
          local.get 2
          call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
          global.get $__memory_base
          i64.const 3816
          i64.add
          i64.load
          local.set 3
        end
        global.get $__memory_base
        i64.const 3928
        i64.add
        local.get 2
        i64.store32
        local.get 1
        local.get 4
        i32.const -1
        i32.add
        i64.extend_i32_u
        i64.const 1
        i64.add
        local.tee 2
        i64.store offset=8
        local.get 3
        local.get 2
        i64.ne
        br_if 1 (;@1;)
        global.get $__memory_base
        local.tee 3
        i64.const 3800
        i64.add
        i64.load
        local.get 5
        i32.const 2
        i32.shr_u
        i32.const 1073741822
        i32.and
        local.tee 4
        i32.const 3
        i32.shl
        i32.store offset=4
        local.get 3
        i64.const 3816
        i64.add
        local.get 3
        i64.const 3808
        i64.add
        i64.load
        local.get 4
        i64.extend_i32_u
        i64.const 3
        i64.shl
        i64.add
        i64.store
        local.get 3
        i64.const 3824
        i64.add
        i64.load
        local.set 3
      end
      local.get 3
      local.get 0
      i64.store
      global.get $__memory_base
      i64.const 3824
      i64.add
      local.get 3
      i64.const 8
      i64.add
      i64.store
      local.get 1
      i64.const 64
      i64.add
      global.set $__stack_pointer
      return
    end
    local.get 1
    i64.const 0
    i64.store offset=32
    local.get 1
    i64.const 8
    i64.add
    local.get 1
    i64.const 16
    i64.add
    call $core::panicking::assert_failed::h294badccb53d6a88
    unreachable)
  (func $motoko_rts::gc::generational::remembered_set::RememberedSet::insert::h13cb425f8573411a (type 16) (param i32)
    (local i64 i64 i64 i32 i32 i32)
    global.get $__stack_pointer
    i64.const 80
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            global.get $__memory_base
            i64.const 3832
            i64.add
            i64.load offset=8
            local.tee 2
            i32.wrap_i64
            local.get 2
            i32.load offset=4
            i32.const 4
            i32.shr_u
            i32.const -1
            i32.add
            local.get 0
            i32.const 3
            i32.shr_u
            i32.and
            i32.const 4
            i32.shl
            i32.add
            i32.const 8
            i32.add
            i64.extend_i32_u
            local.tee 3
            i32.load
            local.tee 4
            i32.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 0
            i32.eq
            br_if 3 (;@1;)
            br 1 (;@3;)
          end
          local.get 3
          i64.const 0
          i64.store offset=8
          local.get 3
          local.get 0
          i32.store
          br 1 (;@2;)
        end
        block  ;; label = @3
          loop  ;; label = @4
            local.get 3
            i64.load offset=8
            local.tee 2
            i64.const 0
            i64.eq
            br_if 1 (;@3;)
            local.get 2
            i64.const 8
            i64.add
            local.set 3
            local.get 2
            i32.load offset=8
            local.get 0
            i32.eq
            br_if 3 (;@1;)
            br 0 (;@4;)
          end
        end
        i32.const 16
        call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
        i64.extend_i32_u
        local.tee 2
        i64.const 17
        i64.add
        i64.const 0
        i64.store
        local.get 2
        i64.const 9
        i64.add
        local.get 0
        i32.store
        local.get 3
        local.get 2
        i64.const 1
        i64.add
        i64.store offset=8
      end
      global.get $__memory_base
      i64.const 3832
      i64.add
      local.tee 2
      i64.const 16
      i64.add
      local.tee 3
      local.get 3
      i32.load
      i32.const 1
      i32.add
      local.tee 0
      i32.store
      local.get 0
      local.get 2
      i64.load offset=8
      local.tee 2
      i32.load offset=4
      i32.const 4
      i32.shr_u
      local.tee 4
      i32.const 65
      i32.mul
      i32.const 100
      i32.div_u
      i32.le_u
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      i32.store offset=4
      local.get 2
      i64.const 8
      i64.add
      i64.const 4294967295
      i64.and
      local.tee 3
      i32.load
      local.set 5
      i32.const 0
      local.set 6
      local.get 1
      i32.const 0
      i32.store offset=24
      local.get 1
      local.get 2
      i64.store offset=8
      local.get 1
      local.get 3
      i64.const 0
      local.get 5
      select
      i64.store offset=16
      global.get $__memory_base
      local.set 2
      local.get 1
      i64.const 8
      i64.add
      call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
      local.get 4
      i32.const 1
      i32.shl
      call $motoko_rts::gc::generational::remembered_set::new_table::h6ee8a4cbe10de291
      local.set 3
      local.get 2
      i64.const 3832
      i64.add
      local.tee 2
      i64.const 16
      i64.add
      i32.const 0
      i32.store
      local.get 2
      local.get 3
      i64.store offset=8
      block  ;; label = @2
        local.get 1
        i64.load offset=16
        local.tee 2
        i64.eqz
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 2
          i32.load
          call $motoko_rts::gc::generational::remembered_set::RememberedSet::insert::h13cb425f8573411a
          local.get 1
          i64.const 0
          local.get 2
          i64.load offset=8
          local.tee 2
          i64.const 8
          i64.add
          local.get 2
          i64.eqz
          select
          i64.store offset=16
          local.get 1
          i64.const 8
          i64.add
          call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
          local.get 1
          i64.load offset=16
          local.tee 2
          i64.eqz
          i32.eqz
          br_if 0 (;@3;)
        end
        global.get $__memory_base
        i64.const 3832
        i64.add
        i64.const 16
        i64.add
        i32.load
        local.set 6
      end
      local.get 6
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      local.get 1
      i64.const 0
      i64.store offset=48
      global.get $__memory_base
      local.tee 2
      i64.const 3832
      i64.add
      i64.const 16
      i64.add
      local.get 1
      i64.const 4
      i64.add
      local.get 1
      i64.const 32
      i64.add
      local.get 2
      i64.const 3320
      i64.add
      call $core::panicking::assert_failed::ha6cca40bd32447e3
      unreachable
    end
    local.get 1
    i64.const 80
    i64.add
    global.set $__stack_pointer)
  (func $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f (type 11) (param i64)
    (local i32 i64 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.load offset=16
      local.tee 1
      local.get 0
      i64.load
      local.tee 2
      i32.load offset=4
      i32.const 4
      i32.shr_u
      local.tee 3
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      i64.load offset=8
      i64.const 0
      i64.ne
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.const 1
      i32.add
      local.tee 4
      local.get 3
      local.get 4
      i32.gt_u
      select
      local.set 5
      local.get 2
      i32.wrap_i64
      i32.const 8
      i32.add
      local.set 4
      block  ;; label = @2
        loop  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 1
            i32.add
            local.tee 1
            local.get 3
            i32.lt_u
            br_if 0 (;@4;)
            i64.const 0
            local.set 2
            local.get 5
            local.set 1
            br 2 (;@2;)
          end
          local.get 1
          i32.const 4
          i32.shl
          local.get 4
          i32.add
          i64.extend_i32_u
          local.tee 2
          i32.load
          i32.eqz
          br_if 0 (;@3;)
        end
      end
      local.get 0
      local.get 2
      i64.store offset=8
      local.get 0
      local.get 1
      i32.store offset=16
    end)
  (func $motoko_rts::gc::generational::remembered_set::new_table::h6ee8a4cbe10de291 (type 18) (param i32) (result i64)
    (local i64 i64 i64 i64 i64)
    local.get 0
    i32.const 4
    i32.shl
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    i64.extend_i32_u
    i64.const 1
    i64.add
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i64.const 8
      i64.add
      local.set 2
      local.get 0
      i64.extend_i32_u
      local.set 3
      i64.const 0
      local.set 4
      loop  ;; label = @2
        local.get 4
        i64.const 4
        i64.shl
        local.get 2
        i64.add
        i64.const 4294967295
        i64.and
        local.tee 5
        i64.const 0
        i64.store offset=8
        local.get 5
        i32.const 0
        i32.store
        local.get 4
        i64.const 1
        i64.add
        local.tee 4
        local.get 3
        i64.ne
        br_if 0 (;@2;)
      end
    end
    local.get 1)
  (func $motoko_rts::gc::generational::write_barrier::init_generational_write_barrier::h30dc7201e6243862 (type 10)
    (local i64 i64 i32)
    global.get $__memory_base
    local.tee 0
    i64.const 3832
    i64.add
    local.tee 1
    i32.const 1024
    call $motoko_rts::gc::generational::remembered_set::new_table::h6ee8a4cbe10de291
    i64.store offset=8
    local.get 1
    i64.const 1
    i64.store
    local.get 1
    i64.const 16
    i64.add
    i32.const 0
    i32.store
    call $get_heap_base
    local.set 2
    local.get 0
    i64.const 3860
    i64.add
    local.get 0
    i64.const 3932
    i64.add
    i32.load
    i32.store
    local.get 0
    i64.const 3856
    i64.add
    local.get 2
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    i32.store)
  (func $motoko_rts::gc::generational::get_limits::ha6736df7ee81ff11 (type 11) (param i64)
    (local i64 i32 i64)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3932
      i64.add
      i32.load
      call $get_heap_base
      i32.const 31
      i32.add
      i32.const -32
      i32.and
      i32.lt_u
      br_if 0 (;@1;)
      global.get $__memory_base
      local.set 1
      call $get_heap_base
      local.set 2
      local.get 1
      i64.const 3932
      i64.add
      i64.load32_u
      local.set 3
      local.get 0
      local.get 1
      i64.const 3928
      i64.add
      i64.load32_u
      i64.store offset=16
      local.get 0
      local.get 3
      i64.store offset=8
      local.get 0
      local.get 2
      i32.const 31
      i32.add
      i32.const -32
      i32.and
      i64.extend_i32_u
      i64.store
      return
    end
    global.get $__memory_base
    local.tee 0
    i64.const 2184
    i64.add
    i64.const 71
    local.get 0
    i64.const 3344
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $motoko_rts::gc::generational::decide_strategy::h7620e853babf9e48 (type 4) (param i64) (result i32)
    (local i64 i64 i64 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.load offset=8
        local.tee 1
        local.get 0
        i64.load
        local.tee 2
        i64.lt_u
        br_if 0 (;@2;)
        local.get 0
        i64.load offset=16
        local.tee 0
        local.get 1
        i64.lt_u
        br_if 1 (;@1;)
        global.get $__memory_base
        local.set 3
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 0
              i64.const 3758096384
              i64.lt_u
              br_if 0 (;@5;)
              i32.const 1
              local.set 4
              local.get 3
              i64.const 3864
              i64.add
              i32.load8_u
              i32.const 1
              i32.and
              i32.eqz
              br_if 1 (;@4;)
            end
            i32.const 1
            local.set 4
            local.get 1
            local.get 2
            i64.sub
            global.get $__memory_base
            i64.const 3368
            i64.add
            i64.load
            i64.gt_u
            br_if 1 (;@3;)
            local.get 0
            local.get 1
            i64.sub
            i64.const 8388609
            i64.lt_u
            i32.const 1
            i32.shl
            return
          end
          global.get $__memory_base
          i64.const 3864
          i64.add
          i32.const 1
          i32.store8
        end
        local.get 4
        return
      end
      global.get $__memory_base
      local.tee 0
      i64.const 2277
      i64.add
      i64.const 49
      local.get 0
      i64.const 3376
      i64.add
      call $core::panicking::panic::h1108c90903a88b24
      unreachable
    end
    global.get $__memory_base
    local.tee 0
    i64.const 2326
    i64.add
    i64.const 49
    local.get 0
    i64.const 3400
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e (type 2) (param i64 i32)
    (local i64 i64 i32 i64)
    global.get $__stack_pointer
    i64.const 64
    i64.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i64.extend_i32_u
        i64.const 1
        i64.add
        local.tee 3
        i32.wrap_i64
        local.tee 1
        local.get 0
        i64.const 16
        i64.const 24
        local.get 0
        i32.load8_u offset=56
        select
        i64.add
        i32.load
        i32.lt_u
        br_if 0 (;@2;)
        local.get 2
        local.get 1
        i32.const 7
        i32.and
        local.tee 4
        i32.store offset=12
        local.get 4
        br_if 1 (;@1;)
        block  ;; label = @3
          global.get $__memory_base
          i64.const 3872
          i64.add
          i64.load
          local.get 3
          i64.const 6
          i64.shr_u
          i64.const 67108863
          i64.and
          i64.add
          local.tee 5
          i32.load8_u
          local.tee 4
          i32.const 1
          local.get 1
          i32.const 3
          i32.shr_u
          i32.const 7
          i32.and
          i32.shl
          local.tee 1
          i32.and
          br_if 0 (;@3;)
          local.get 5
          local.get 4
          local.get 1
          i32.or
          i32.store8
          local.get 3
          i64.const 4294967295
          i64.and
          local.tee 3
          call $motoko_rts::gc::generational::mark_stack::push_mark_stack::h75ccea4977ad5e46
          local.get 3
          call $motoko_rts::types::block_size::h0bf0d784741e3e30
          local.set 1
          local.get 0
          local.get 0
          i64.load offset=48
          local.get 1
          i32.const 3
          i32.shl
          i64.extend_i32_u
          i64.add
          i64.store offset=48
        end
        local.get 2
        i64.const 64
        i64.add
        global.set $__stack_pointer
        return
      end
      global.get $__memory_base
      local.tee 0
      i64.const 2439
      i64.add
      i64.const 58
      local.get 0
      i64.const 3496
      i64.add
      call $core::panicking::panic::h1108c90903a88b24
      unreachable
    end
    local.get 2
    i64.const 0
    i64.store offset=32
    local.get 2
    i64.const 12
    i64.add
    global.get $__memory_base
    local.tee 0
    i64.const 1836
    i64.add
    local.get 2
    i64.const 16
    i64.add
    local.get 0
    i64.const 3520
    i64.add
    call $core::panicking::assert_failed::ha6cca40bd32447e3
    unreachable)
  (func $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b (type 3) (param i64 i64)
    block  ;; label = @1
      local.get 0
      i64.const 16
      i64.const 24
      local.get 0
      i32.load8_u offset=56
      select
      i64.add
      i64.load
      local.get 1
      i64.load32_u
      i64.const 1
      i64.add
      local.tee 0
      i64.gt_u
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      i32.load
      i32.store
      local.get 0
      local.get 1
      i64.store32
      return
    end
    global.get $__memory_base
    local.tee 1
    i64.const 2727
    i64.add
    i64.const 50
    local.get 1
    i64.const 3688
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $motoko_rts::gc::mark_compact::bitmap::alloc_bitmap::h15bb744d451f0bbd (type 19) (param i32 i32)
    (local i64 i64)
    local.get 0
    i32.const 7
    i32.add
    i32.const 3
    i32.shr_u
    i32.const 7
    i32.add
    i32.const 3
    i32.shr_u
    i32.const 7
    i32.add
    i32.const 268435448
    i32.and
    local.tee 0
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    i64.extend_i32_u
    i64.const 9
    i64.add
    local.tee 2
    i32.const 0
    local.get 0
    i64.extend_i32_u
    memory.fill
    global.get $__memory_base
    local.tee 3
    i64.const 3872
    i64.add
    local.get 2
    local.get 1
    i32.const 3
    i32.shr_u
    i64.extend_i32_u
    i64.sub
    i64.store
    local.get 3
    i64.const 3880
    i64.add
    local.get 2
    i64.store)
  (func $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705 (type 4) (param i64) (result i32)
    (local i32 i32 i32 i64 i64)
    i32.const -1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.load offset=12
      local.tee 2
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.eq
      br_if 0 (;@1;)
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i64.load
          local.tee 4
          i64.const 0
          i64.ne
          br_if 0 (;@3;)
          global.get $__memory_base
          i64.const 3872
          i64.add
          i64.load
          local.set 5
          local.get 0
          i32.load offset=16
          local.set 1
          loop  ;; label = @4
            block  ;; label = @5
              local.get 2
              local.get 1
              i32.add
              local.tee 2
              local.get 3
              i32.ne
              br_if 0 (;@5;)
              i32.const -1
              local.set 1
              br 3 (;@2;)
            end
            local.get 0
            local.get 5
            local.get 2
            i32.const 3
            i32.shr_u
            i64.extend_i32_u
            i64.add
            i64.load align=1
            local.tee 4
            i64.store
            local.get 0
            local.get 4
            i64.clz
            i32.wrap_i64
            local.tee 1
            i32.store offset=16
            local.get 4
            i64.eqz
            br_if 0 (;@4;)
          end
        end
        local.get 0
        local.get 4
        local.get 4
        i64.ctz
        local.tee 5
        i64.shr_u
        i64.const 1
        i64.shr_u
        i64.store
        local.get 2
        local.get 5
        i32.wrap_i64
        i32.add
        local.tee 1
        i32.const 1
        i32.add
        local.set 3
      end
      local.get 0
      local.get 3
      i32.store offset=12
    end
    local.get 1)
  (func $motoko_rts::gc::mark_compact::mark_stack::push_mark_stack::hccc6b644da04914e (type 2) (param i64 i32)
    (local i64 i64 i64 i32 i64)
    block  ;; label = @1
      global.get $__memory_base
      local.tee 2
      i64.const 3912
      i64.add
      i64.load
      local.tee 3
      local.get 2
      i64.const 3904
      i64.add
      i64.load
      i64.ne
      br_if 0 (;@1;)
      block  ;; label = @2
        global.get $__memory_base
        local.tee 2
        i64.const 3888
        i64.add
        i64.load
        local.tee 4
        i32.load offset=4
        i32.const 7
        i32.add
        local.tee 5
        i32.const -8
        i32.and
        i64.extend_i32_u
        local.get 2
        i64.const 3928
        i64.add
        i64.load32_u
        i64.add
        local.tee 6
        memory.size
        i64.const 16
        i64.shl
        i64.le_u
        br_if 0 (;@2;)
        local.get 6
        call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
        global.get $__memory_base
        local.tee 2
        i64.const 3912
        i64.add
        i64.load
        local.set 3
        local.get 2
        i64.const 3888
        i64.add
        i64.load
        local.set 4
      end
      global.get $__memory_base
      local.tee 2
      i64.const 3928
      i64.add
      local.get 6
      i64.store32
      local.get 4
      local.get 5
      i32.const 2
      i32.shr_u
      i32.const 1073741822
      i32.and
      local.tee 5
      i32.const 3
      i32.shl
      i32.store offset=4
      local.get 2
      i64.const 3904
      i64.add
      local.get 2
      i64.const 3896
      i64.add
      i64.load
      local.get 5
      i64.extend_i32_u
      i64.const 3
      i64.shl
      i64.add
      i64.store
    end
    local.get 3
    local.get 0
    i64.store
    local.get 3
    local.get 1
    i64.extend_i32_u
    i64.store offset=8
    global.get $__memory_base
    i64.const 3912
    i64.add
    local.get 3
    i64.const 16
    i64.add
    i64.store)
  (func $initialize_compacting_gc (type 10)
    (local i64 i32)
    global.get $__memory_base
    local.tee 0
    i64.const 3932
    i64.add
    call $get_heap_base
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    local.tee 1
    i32.store
    local.get 0
    i64.const 3928
    i64.add
    local.get 1
    i32.store)
  (func $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca (type 16) (param i32)
    (local i64 i64 i32 i32)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3872
      i64.add
      i64.load
      local.get 0
      i64.extend_i32_u
      i64.const 1
      i64.add
      local.tee 1
      i64.const 6
      i64.shr_u
      i64.const 67108863
      i64.and
      i64.add
      local.tee 2
      i32.load8_u
      local.tee 0
      i32.const 1
      local.get 1
      i32.wrap_i64
      i32.const 3
      i32.shr_u
      i32.const 7
      i32.and
      i32.shl
      local.tee 3
      i32.and
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 4
      local.get 2
      local.get 0
      local.get 3
      i32.or
      i32.store8
      local.get 1
      i64.const 4294967295
      i64.and
      local.get 4
      call $motoko_rts::gc::mark_compact::mark_stack::push_mark_stack::hccc6b644da04914e
    end)
  (func $get_reclaimed (type 20) (result i64)
    global.get $__memory_base
    i64.const 3920
    i64.add
    i64.load)
  (func $get_total_allocations (type 20) (result i64)
    (local i64 i32 i32)
    global.get $__memory_base
    local.tee 0
    i64.const 3928
    i64.add
    i32.load
    local.set 1
    call $get_heap_base
    local.set 2
    local.get 0
    i64.const 3920
    i64.add
    i64.load
    local.get 1
    local.get 2
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    i32.sub
    i64.extend_i32_u
    i64.add)
  (func $get_heap_size (type 9) (result i32)
    global.get $__memory_base
    i64.const 3928
    i64.add
    i32.load
    call $get_heap_base
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    i32.sub)
  (func $alloc_blob (type 7) (param i32) (result i32)
    local.get 0
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60)
  (func $alloc_array (type 7) (param i32) (result i32)
    local.get 0
    call $motoko_rts::memory::alloc_array::h2a91ff2be1084c58)
  (func $motoko_rts::principal_id::principal_of_blob::hf139e83611204bf7 (type 7) (param i32) (result i32)
    (local i64 i32 i64 i32 i64 i64 i64 i32 i32 i64)
    global.get $__stack_pointer
    i64.const 32
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 0
    call $compute_crc32
    local.set 2
    local.get 0
    i64.extend_i32_u
    local.tee 3
    i64.const 5
    i64.add
    i32.load
    local.tee 0
    i32.const 8
    i32.add
    i32.const 5
    i32.div_u
    i32.const 3
    i32.shl
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    local.set 4
    local.get 1
    i64.const 0
    i64.store offset=24
    local.get 1
    i64.const 21474836488
    i64.store offset=16
    local.get 1
    local.get 4
    i64.extend_i32_u
    local.tee 5
    i64.const 9
    i64.add
    local.tee 6
    i64.store offset=8
    local.get 1
    i64.const 8
    i64.add
    local.get 2
    i32.const 24
    i32.shr_u
    call $motoko_rts::principal_id::enc_stash::h9337051efa825169
    local.get 1
    i64.const 8
    i64.add
    local.get 2
    i32.const 16
    i32.shr_u
    call $motoko_rts::principal_id::enc_stash::h9337051efa825169
    local.get 1
    i64.const 8
    i64.add
    local.get 2
    i32.const 8
    i32.shr_u
    call $motoko_rts::principal_id::enc_stash::h9337051efa825169
    local.get 1
    i64.const 8
    i64.add
    local.get 2
    call $motoko_rts::principal_id::enc_stash::h9337051efa825169
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i64.const 1
      i64.add
      i64.const 8
      i64.add
      local.set 3
      i32.const 0
      local.set 2
      loop  ;; label = @2
        local.get 1
        i64.const 8
        i64.add
        local.get 3
        i32.load8_u
        call $motoko_rts::principal_id::enc_stash::h9337051efa825169
        local.get 3
        i64.const 1
        i64.add
        local.set 3
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        local.get 0
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 5
    i64.const 1
    i64.add
    local.set 3
    block  ;; label = @1
      local.get 1
      i32.load offset=28
      local.tee 2
      i32.eqz
      br_if 0 (;@1;)
      global.get $__memory_base
      local.set 5
      local.get 1
      i64.load offset=8
      local.tee 7
      local.get 5
      i64.const 1704
      i64.add
      local.get 1
      i32.load offset=24
      local.get 1
      i32.load offset=20
      local.get 2
      i32.sub
      i32.shl
      i64.extend_i32_u
      i64.const 31
      i64.and
      i64.add
      i32.load8_u
      i32.store8
      local.get 3
      local.get 7
      local.get 6
      i64.sub
      i32.wrap_i64
      i32.const 1
      i32.add
      call $motoko_rts::types::Blob::shrink::hf021b0a99a21983f
    end
    local.get 3
    i32.load offset=4
    local.tee 8
    i32.const 4
    i32.add
    i32.const 5
    i32.div_u
    i32.const 6
    i32.mul
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    local.tee 9
    i64.extend_i32_u
    local.tee 3
    i64.const 1
    i64.add
    local.set 10
    local.get 3
    i64.const 9
    i64.add
    local.tee 7
    local.set 5
    block  ;; label = @1
      local.get 8
      i32.eqz
      br_if 0 (;@1;)
      i32.const 0
      local.set 4
      local.get 7
      local.set 5
      i32.const 0
      local.set 2
      loop  ;; label = @2
        local.get 5
        local.tee 3
        local.get 6
        i32.load8_u
        local.tee 0
        i32.const 32
        i32.add
        local.get 0
        local.get 0
        i32.const -65
        i32.add
        i32.const 255
        i32.and
        i32.const 26
        i32.lt_u
        select
        i32.store8
        local.get 3
        i64.const 1
        i64.add
        local.set 5
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        block  ;; label = @3
          local.get 4
          i32.const 1
          i32.add
          local.tee 4
          i32.const 5
          i32.rem_s
          br_if 0 (;@3;)
          local.get 2
          local.get 8
          i32.ge_u
          br_if 0 (;@3;)
          local.get 3
          i32.const 45
          i32.store8 offset=1
          local.get 3
          i64.const 2
          i64.add
          local.set 5
          i32.const 0
          local.set 4
        end
        local.get 6
        i64.const 1
        i64.add
        local.set 6
        local.get 2
        local.get 8
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 10
    local.get 5
    i32.wrap_i64
    local.get 7
    i32.wrap_i64
    i32.sub
    call $motoko_rts::types::Blob::shrink::hf021b0a99a21983f
    local.get 1
    i64.const 32
    i64.add
    global.set $__stack_pointer
    local.get 9)
  (func $principal_of_blob (type 7) (param i32) (result i32)
    local.get 0
    call $motoko_rts::principal_id::principal_of_blob::hf139e83611204bf7)
  (func $blob_of_principal (type 7) (param i32) (result i32)
    (local i64 i32 i64 i64 i64 i32 i32 i32)
    local.get 0
    call $motoko_rts::text::blob_of_text::h1509dd78415f3bfb
    i64.extend_i32_u
    local.tee 1
    i64.const 5
    i64.add
    i32.load
    local.tee 2
    i32.const 7
    i32.add
    i32.const 3
    i32.shr_u
    i32.const 5
    i32.mul
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    i64.extend_i32_u
    local.tee 3
    i64.const 9
    i64.add
    local.tee 4
    local.set 5
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i64.const 1
      i64.add
      i64.const 8
      i64.add
      local.set 1
      local.get 4
      local.set 5
      i32.const 0
      local.set 6
      i32.const 0
      local.set 0
      i32.const 0
      local.set 7
      loop  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load8_u
              local.tee 8
              i32.const 122
              i32.gt_u
              br_if 0 (;@5;)
              local.get 8
              i32.const 16
              i32.or
              i32.const 61
              i32.eq
              br_if 2 (;@3;)
              block  ;; label = @6
                local.get 8
                i32.const -65
                i32.add
                i32.const 255
                i32.and
                i32.const 26
                i32.lt_u
                br_if 0 (;@6;)
                block  ;; label = @7
                  local.get 8
                  i32.const 96
                  i32.gt_u
                  br_if 0 (;@7;)
                  local.get 8
                  i32.const -23
                  i32.add
                  i32.const 0
                  local.get 8
                  i32.const -50
                  i32.add
                  i32.const 255
                  i32.and
                  i32.const 6
                  i32.lt_u
                  select
                  local.set 8
                  br 3 (;@4;)
                end
                local.get 8
                i32.const -96
                i32.add
                local.set 8
                br 2 (;@4;)
              end
              local.get 8
              i32.const -64
              i32.add
              local.set 8
              br 1 (;@4;)
            end
            global.get $__memory_base
            i64.const 1736
            i64.add
            i64.const 40
            call $motoko_rts::rts_trap_with::h000639acaf03dda1
            unreachable
          end
          local.get 8
          i32.const -1
          i32.add
          i32.const 255
          i32.and
          local.tee 8
          i32.const 32
          i32.ge_u
          br_if 0 (;@3;)
          local.get 7
          i32.const 5
          i32.shl
          local.get 8
          i32.or
          local.set 7
          local.get 0
          i32.const 3
          i32.lt_u
          local.set 8
          local.get 0
          i32.const 5
          i32.add
          local.set 0
          local.get 8
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 5
            local.get 7
            local.get 0
            i32.const -8
            i32.add
            local.tee 0
            i32.shr_u
            i32.store8
            local.get 5
            i64.const 1
            i64.add
            local.set 5
            local.get 7
            i32.const -1
            local.get 0
            i32.shl
            i32.const -1
            i32.xor
            i32.and
            local.set 7
            local.get 0
            i32.const 7
            i32.gt_u
            br_if 0 (;@4;)
          end
        end
        local.get 1
        i64.const 1
        i64.add
        local.set 1
        local.get 6
        i32.const 1
        i32.add
        local.tee 6
        local.get 2
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 3
    i64.const 1
    i64.add
    local.tee 1
    local.get 5
    local.get 4
    i64.sub
    i32.wrap_i64
    call $motoko_rts::types::Blob::shrink::hf021b0a99a21983f
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      local.tee 0
      i32.const 4
      i32.ge_u
      br_if 0 (;@1;)
      global.get $__memory_base
      i64.const 2912
      i64.add
      i64.const 38
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    local.get 0
    i32.const -4
    i32.add
    local.tee 0
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    local.tee 7
    i64.extend_i32_u
    i64.const 9
    i64.add
    local.get 1
    i64.const 12
    i64.add
    local.get 0
    i64.extend_i32_u
    memory.copy
    local.get 7
    call $motoko_rts::principal_id::principal_of_blob::hf139e83611204bf7
    drop
    global.get $__memory_base
    local.tee 5
    i64.const 1807
    i64.add
    i64.const 15
    local.get 5
    i64.const 3248
    i64.add
    call $core::panicking::panic::h1108c90903a88b24
    unreachable)
  (func $motoko_rts::text::blob_of_text::h1509dd78415f3bfb (type 7) (param i32) (result i32)
    (local i64 i32)
    block  ;; label = @1
      local.get 0
      i64.extend_i32_u
      local.tee 1
      i32.load offset=1
      i32.const 17
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      return
    end
    local.get 0
    local.get 1
    i64.const 5
    i64.add
    i32.load
    call $motoko_rts::text::alloc_text_blob::h11ccee3a71497588
    local.tee 2
    i64.extend_i32_u
    i64.const 9
    i64.add
    call $text_to_buf
    local.get 2)
  (func $text_of_ptr_size (type 21) (param i64 i32) (result i32)
    (local i32)
    local.get 1
    call $motoko_rts::text::alloc_text_blob::h11ccee3a71497588
    local.tee 2
    i64.extend_i32_u
    i64.const 9
    i64.add
    local.get 0
    local.get 1
    i64.extend_i32_u
    memory.copy
    local.get 2)
  (func $text_concat (type 14) (param i32 i32) (result i32)
    (local i64 i32 i64 i32 i32 i64)
    block  ;; label = @1
      local.get 0
      i64.extend_i32_u
      local.tee 2
      i64.const 5
      i64.add
      i32.load
      local.tee 3
      br_if 0 (;@1;)
      local.get 1
      return
    end
    block  ;; label = @1
      local.get 1
      i64.extend_i32_u
      local.tee 4
      i64.const 5
      i64.add
      i32.load
      local.tee 5
      br_if 0 (;@1;)
      local.get 0
      return
    end
    block  ;; label = @1
      local.get 5
      local.get 3
      i32.add
      local.tee 6
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 6
        i32.const 1073741823
        i32.gt_u
        br_if 0 (;@2;)
        memory.size
        local.set 4
        block  ;; label = @3
          global.get $__memory_base
          i64.const 3928
          i64.add
          i32.load
          local.tee 3
          i64.extend_i32_u
          i64.const 16
          i64.add
          local.tee 2
          local.get 4
          i64.const 16
          i64.shl
          i64.le_u
          br_if 0 (;@3;)
          local.get 2
          call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
        end
        global.get $__memory_base
        i64.const 3928
        i64.add
        local.get 2
        i64.store32
        local.get 3
        i32.const -1
        i32.add
        local.tee 3
        i64.extend_i32_u
        local.tee 2
        i32.const 25
        i32.store offset=1
        local.get 2
        i64.const 13
        i64.add
        local.get 1
        i32.store
        local.get 2
        i64.const 9
        i64.add
        local.get 0
        i32.store
        local.get 2
        i64.const 5
        i64.add
        local.get 6
        i32.store
        local.get 3
        return
      end
      global.get $__memory_base
      i64.const 2950
      i64.add
      i64.const 27
      call $motoko_rts::rts_trap_with::h000639acaf03dda1
      unreachable
    end
    local.get 6
    call $motoko_rts::text::alloc_text_blob::h11ccee3a71497588
    local.tee 0
    i64.extend_i32_u
    i64.const 9
    i64.add
    local.tee 7
    local.get 2
    i64.const 1
    i64.add
    i64.const 8
    i64.add
    local.get 3
    i64.extend_i32_u
    local.tee 2
    memory.copy
    local.get 7
    local.get 2
    i64.add
    local.get 4
    i64.const 1
    i64.add
    i64.const 8
    i64.add
    local.get 5
    i64.extend_i32_u
    memory.copy
    local.get 0)
  (func $blob_of_text (type 7) (param i32) (result i32)
    local.get 0
    call $motoko_rts::text::blob_of_text::h1509dd78415f3bfb)
  (func $text_singleton (type 7) (param i32) (result i32)
    (local i64 i32)
    global.get $__stack_pointer
    i64.const 16
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 128
            i32.lt_u
            br_if 0 (;@4;)
            local.get 0
            i32.const 2048
            i32.lt_u
            br_if 1 (;@3;)
            local.get 0
            i32.const 65536
            i32.ge_u
            br_if 2 (;@2;)
            local.get 1
            local.get 0
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 1
            local.get 0
            i32.const 12
            i32.shr_u
            i32.const 224
            i32.or
            i32.store8 offset=12
            local.get 1
            local.get 0
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            i32.const 3
            local.set 0
            br 3 (;@1;)
          end
          local.get 1
          local.get 0
          i32.store8 offset=12
          i32.const 1
          local.set 0
          br 2 (;@1;)
        end
        local.get 1
        local.get 0
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 1
        local.get 0
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 0
        br 1 (;@1;)
      end
      local.get 1
      local.get 0
      i32.const 63
      i32.and
      i32.const 128
      i32.or
      i32.store8 offset=15
      local.get 1
      local.get 0
      i32.const 18
      i32.shr_u
      i32.const 240
      i32.or
      i32.store8 offset=12
      local.get 1
      local.get 0
      i32.const 6
      i32.shr_u
      i32.const 63
      i32.and
      i32.const 128
      i32.or
      i32.store8 offset=14
      local.get 1
      local.get 0
      i32.const 12
      i32.shr_u
      i32.const 63
      i32.and
      i32.const 128
      i32.or
      i32.store8 offset=13
      i32.const 4
      local.set 0
    end
    local.get 0
    call $motoko_rts::text::alloc_text_blob::h11ccee3a71497588
    local.tee 2
    i64.extend_i32_u
    i64.const 9
    i64.add
    local.get 1
    i64.const 12
    i64.add
    local.get 0
    i64.extend_i32_u
    memory.copy
    local.get 1
    i64.const 16
    i64.add
    global.set $__stack_pointer
    local.get 2)
  (func $text_iter (type 7) (param i32) (result i32)
    (local i32 i64 i64)
    i32.const 3
    call $motoko_rts::memory::alloc_array::h2a91ff2be1084c58
    local.tee 1
    i64.extend_i32_u
    local.tee 2
    i64.const 17
    i64.add
    local.tee 3
    i32.const 0
    i32.store
    local.get 2
    i64.const 9
    i64.add
    local.tee 2
    local.get 0
    local.get 3
    call $motoko_rts::text_iter::find_leaf::h5cbf944c560302e0
    i32.store
    local.get 2
    i32.wrap_i64
    call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
    local.get 1)
  (func $text_iter_next (type 7) (param i32) (result i32)
    (local i64 i32 i64 i64 i64 i64 i32 i32 i64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.extend_i32_u
        local.tee 1
        i64.const 17
        i64.add
        i32.load
        i32.const 1
        i32.shr_u
        local.tee 2
        local.get 1
        i64.const 9
        i64.add
        local.tee 3
        i64.load32_u
        local.tee 4
        i64.const 5
        i64.add
        i32.load
        i32.ge_u
        br_if 0 (;@2;)
        local.get 4
        i64.const 1
        i64.add
        local.set 1
        br 1 (;@1;)
      end
      local.get 1
      i64.const 1
      i64.add
      i64.const 16
      i64.add
      local.set 5
      local.get 3
      i64.const 16
      i64.add
      local.tee 6
      i32.wrap_i64
      local.set 7
      local.get 3
      i32.wrap_i64
      local.set 8
      local.get 3
      i64.const 8
      i64.add
      local.set 4
      loop  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 6
            i64.load32_u
            i64.const 9
            i64.add
            local.tee 1
            i32.load
            local.tee 0
            i64.extend_i32_u
            local.tee 9
            i32.load offset=1
            i32.const 25
            i32.eq
            br_if 0 (;@4;)
            local.get 3
            local.get 0
            i32.store
            local.get 8
            call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
            local.get 4
            i32.const 0
            i32.store
            local.get 6
            local.get 1
            i32.load offset=8
            i32.store
            local.get 7
            local.set 0
            br 1 (;@3;)
          end
          local.get 1
          local.get 9
          i64.const 13
          i64.add
          i32.load
          i32.store
          local.get 1
          i32.wrap_i64
          call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
          local.get 4
          i32.const 0
          i32.store
          local.get 3
          local.get 9
          i64.const 9
          i64.add
          i32.load
          local.get 5
          call $motoko_rts::text_iter::find_leaf::h5cbf944c560302e0
          i32.store
          local.get 8
          local.set 0
        end
        local.get 0
        call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334
        local.get 4
        i32.load
        i32.const 1
        i32.shr_u
        local.tee 2
        local.get 3
        i64.load32_u
        local.tee 1
        i64.const 5
        i64.add
        i32.load
        i32.ge_u
        br_if 0 (;@2;)
      end
      local.get 1
      i64.const 1
      i64.add
      local.set 1
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 2
        i64.extend_i32_u
        i64.add
        i64.const 8
        i64.add
        local.tee 4
        i32.load8_u
        local.tee 0
        i32.const 255
        i32.xor
        i32.clz
        i32.const -24
        i32.add
        local.tee 8
        br_if 0 (;@2;)
        i32.const 1
        local.set 7
        br 1 (;@1;)
      end
      i32.const 255
      local.get 8
      i32.const 7
      i32.and
      i32.shr_u
      local.get 0
      i32.and
      local.set 0
      i32.const 1
      local.set 7
      local.get 8
      i32.const 1
      i32.eq
      br_if 0 (;@1;)
      local.get 8
      i32.const 255
      i32.and
      local.set 7
      local.get 8
      i64.extend_i32_u
      i64.const 255
      i64.and
      local.set 9
      i64.const 1
      local.set 1
      loop  ;; label = @2
        local.get 0
        i32.const 6
        i32.shl
        local.get 4
        local.get 1
        i64.add
        i32.load8_u
        i32.const 63
        i32.and
        i32.or
        local.set 0
        local.get 1
        i64.const 1
        i64.add
        local.tee 1
        local.get 9
        i64.ne
        br_if 0 (;@2;)
      end
    end
    local.get 3
    i64.const 8
    i64.add
    local.get 7
    local.get 2
    i32.add
    i32.const 1
    i32.shl
    i32.store
    local.get 0)
  (func $version (type 9) (result i32)
    (local i32 i64 i64)
    i32.const 3
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    local.tee 0
    i64.extend_i32_u
    local.tee 1
    i64.const 11
    i64.add
    global.get $__memory_base
    i64.const 2977
    i64.add
    local.tee 2
    i64.const 2
    i64.add
    i32.load8_u
    i32.store8
    local.get 1
    i64.const 9
    i64.add
    local.get 2
    i32.load16_u align=1
    i32.store16 align=1
    local.get 0)
  (func $alloc_words (type 7) (param i32) (result i32)
    (local i32 i64)
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3928
      i64.add
      i32.load
      local.tee 1
      i64.extend_i32_u
      local.get 0
      i32.const 3
      i32.shl
      i64.extend_i32_u
      i64.add
      local.tee 2
      memory.size
      i64.const 16
      i64.shl
      i64.le_u
      br_if 0 (;@1;)
      local.get 2
      call $motoko_rts::memory::ic::linear_memory::_$LT$impl$u20$motoko_rts..memory..Memory$u20$for$u20$motoko_rts..memory..ic..IcMemory$GT$::grow_memory::h748783b48a26272f
    end
    global.get $__memory_base
    i64.const 3928
    i64.add
    local.get 2
    i64.store32
    local.get 1
    i32.const -1
    i32.add)
  (func $motoko_rts::gc::mark_compact::compacting_gc::hedb61db8ff561f85 (type 10)
    (local i64 i32 i64 i32 i32 i32 i32 i64 i64 i64 i64 i64 i64 i32)
    global.get $__stack_pointer
    i64.const 32
    i64.sub
    local.tee 0
    global.set $__stack_pointer
    call $get_heap_base
    local.set 1
    global.get $__memory_base
    local.set 2
    call $get_static_roots
    local.set 3
    local.get 2
    i64.const 3928
    i64.add
    i32.load
    local.tee 4
    local.get 1
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    local.tee 5
    i32.sub
    local.tee 6
    local.get 5
    i32.const 3
    i32.shr_u
    call $motoko_rts::gc::mark_compact::bitmap::alloc_bitmap::h15bb744d451f0bbd
    local.get 2
    i64.const 3896
    i64.add
    i32.const 512
    call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
    i64.extend_i32_u
    local.tee 7
    i64.const 9
    i64.add
    local.tee 8
    i64.store
    local.get 2
    i64.const 3888
    i64.add
    local.get 7
    i64.const 1
    i64.add
    i64.store
    local.get 2
    i64.const 3912
    i64.add
    local.get 8
    i64.store
    local.get 2
    i64.const 3904
    i64.add
    local.get 7
    i64.const 521
    i64.add
    i64.store
    block  ;; label = @1
      local.get 3
      i64.extend_i32_u
      local.tee 2
      i64.const 5
      i64.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i64.const 1
      i64.add
      i64.const 8
      i64.add
      local.set 8
      local.get 1
      i64.extend_i32_u
      local.set 9
      local.get 5
      i64.extend_i32_u
      local.set 10
      i64.const 0
      local.set 2
      loop  ;; label = @2
        block  ;; label = @3
          local.get 2
          i64.const 3
          i64.shl
          i64.const 4294967288
          i64.and
          local.get 8
          i64.add
          i64.load32_u
          i64.const 5
          i64.add
          local.tee 7
          i32.load
          local.tee 1
          i32.const 1
          i32.eq
          br_if 0 (;@3;)
          local.get 1
          i32.const 1
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i64.extend_i32_u
          i64.const 1
          i64.add
          local.get 10
          i64.lt_u
          br_if 0 (;@3;)
          local.get 1
          call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
          local.get 7
          local.get 7
          i64.load32_u
          local.tee 11
          i32.load offset=1
          i32.store
          local.get 11
          local.get 7
          i64.store32 offset=1
        end
        local.get 2
        i64.const 1
        i64.add
        local.tee 2
        local.get 9
        i64.ne
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3784
      i64.add
      i32.load
      local.tee 1
      i32.const 1
      i32.eq
      br_if 0 (;@1;)
      local.get 1
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
      global.get $__memory_base
      i64.const 3784
      i64.add
      local.tee 2
      local.get 2
      i64.load32_u
      local.tee 7
      i32.load offset=1
      i32.store
      local.get 7
      local.get 2
      i64.store32 offset=1
    end
    block  ;; label = @1
      global.get $__memory_base
      local.tee 7
      i64.const 3912
      i64.add
      i64.load
      local.tee 2
      local.get 7
      i64.const 3896
      i64.add
      i64.load
      i64.eq
      br_if 0 (;@1;)
      local.get 5
      i64.extend_i32_u
      local.set 8
      loop  ;; label = @2
        global.get $__memory_base
        i64.const 3912
        i64.add
        local.get 2
        i64.const -16
        i64.add
        local.tee 7
        i64.store
        local.get 7
        i64.load
        local.set 7
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    local.get 2
                                    i64.const -8
                                    i64.add
                                    i32.load
                                    local.tee 1
                                    i32.const -1
                                    i32.add
                                    br_table 0 (;@16;) 1 (;@15;) 4 (;@12;) 1 (;@15;) 12 (;@4;) 1 (;@15;) 13 (;@3;) 1 (;@15;) 9 (;@7;) 1 (;@15;) 8 (;@8;) 1 (;@15;) 7 (;@9;) 1 (;@15;) 6 (;@10;) 1 (;@15;) 13 (;@3;) 1 (;@15;) 2 (;@14;) 1 (;@15;) 13 (;@3;) 1 (;@15;) 13 (;@3;) 1 (;@15;) 5 (;@11;) 1 (;@15;) 3 (;@13;) 1 (;@15;) 2 (;@14;) 1 (;@15;) 2 (;@14;) 1 (;@15;)
                                  end
                                  local.get 7
                                  i32.load offset=4
                                  local.tee 1
                                  i32.eqz
                                  br_if 12 (;@3;)
                                  local.get 7
                                  i64.const 12
                                  i64.add
                                  local.set 10
                                  local.get 1
                                  i64.extend_i32_u
                                  local.set 12
                                  i64.const 0
                                  local.set 2
                                  loop  ;; label = @16
                                    block  ;; label = @17
                                      local.get 10
                                      local.get 2
                                      i64.const 2
                                      i64.shl
                                      i64.add
                                      local.tee 9
                                      i32.load
                                      local.tee 1
                                      i32.const 1
                                      i32.eq
                                      br_if 0 (;@17;)
                                      local.get 1
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 1
                                      i64.extend_i32_u
                                      local.tee 11
                                      i64.const 1
                                      i64.add
                                      local.get 8
                                      i64.lt_u
                                      br_if 0 (;@17;)
                                      local.get 1
                                      call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                                      local.get 7
                                      local.get 11
                                      i64.le_u
                                      br_if 0 (;@17;)
                                      local.get 9
                                      local.get 9
                                      i64.load32_u
                                      local.tee 11
                                      i32.load offset=1
                                      i32.store
                                      local.get 11
                                      local.get 9
                                      i64.store32 offset=1
                                    end
                                    local.get 2
                                    i64.const 1
                                    i64.add
                                    local.tee 2
                                    local.get 12
                                    i64.ne
                                    br_if 0 (;@16;)
                                    br 13 (;@3;)
                                  end
                                end
                                local.get 1
                                i32.const 31
                                i32.gt_u
                                br_if 10 (;@4;)
                              end
                              global.get $__memory_base
                              i64.const 1902
                              i64.add
                              i64.const 42
                              call $motoko_rts::rts_trap_with::h000639acaf03dda1
                              unreachable
                            end
                            global.get $__memory_base
                            i64.const 1944
                            i64.add
                            i64.const 51
                            call $motoko_rts::rts_trap_with::h000639acaf03dda1
                            unreachable
                          end
                          local.get 7
                          i32.load offset=4
                          local.tee 1
                          i32.const 1
                          i32.eq
                          br_if 8 (;@3;)
                          local.get 1
                          i32.const 1
                          i32.and
                          i32.eqz
                          br_if 8 (;@3;)
                          local.get 1
                          i64.extend_i32_u
                          local.tee 2
                          i64.const 1
                          i64.add
                          local.get 8
                          i64.lt_u
                          br_if 8 (;@3;)
                          local.get 1
                          call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                          local.get 7
                          local.get 2
                          i64.le_u
                          br_if 8 (;@3;)
                          br 5 (;@6;)
                        end
                        block  ;; label = @11
                          local.get 7
                          i32.load offset=8
                          local.tee 1
                          i32.const 1
                          i32.eq
                          br_if 0 (;@11;)
                          local.get 1
                          i32.const 1
                          i32.and
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 1
                          i64.extend_i32_u
                          local.tee 2
                          i64.const 1
                          i64.add
                          local.get 8
                          i64.lt_u
                          br_if 0 (;@11;)
                          local.get 1
                          call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                          local.get 7
                          local.get 2
                          i64.le_u
                          br_if 0 (;@11;)
                          local.get 7
                          i64.const 8
                          i64.add
                          local.tee 2
                          local.get 2
                          i64.load32_u
                          local.tee 9
                          i32.load offset=1
                          i32.store
                          local.get 9
                          local.get 2
                          i64.store32 offset=1
                        end
                        local.get 7
                        i32.load offset=12
                        local.tee 1
                        i32.const 1
                        i32.eq
                        br_if 7 (;@3;)
                        local.get 1
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 7 (;@3;)
                        local.get 1
                        i64.extend_i32_u
                        local.tee 2
                        i64.const 1
                        i64.add
                        local.get 8
                        i64.lt_u
                        br_if 7 (;@3;)
                        local.get 1
                        call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                        local.get 7
                        local.get 2
                        i64.le_u
                        br_if 7 (;@3;)
                        local.get 7
                        i64.const 12
                        i64.add
                        local.set 2
                        br 5 (;@5;)
                      end
                      local.get 7
                      i32.load offset=8
                      local.tee 1
                      i32.const 1
                      i32.eq
                      br_if 6 (;@3;)
                      local.get 1
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 6 (;@3;)
                      local.get 1
                      i64.extend_i32_u
                      local.tee 2
                      i64.const 1
                      i64.add
                      local.get 8
                      i64.lt_u
                      br_if 6 (;@3;)
                      local.get 1
                      call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                      local.get 7
                      local.get 2
                      i64.le_u
                      br_if 6 (;@3;)
                      local.get 7
                      i64.const 8
                      i64.add
                      local.set 2
                      br 4 (;@5;)
                    end
                    local.get 7
                    i32.load offset=4
                    local.tee 1
                    i32.const 1
                    i32.eq
                    br_if 5 (;@3;)
                    local.get 1
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 5 (;@3;)
                    local.get 1
                    i64.extend_i32_u
                    local.tee 2
                    i64.const 1
                    i64.add
                    local.get 8
                    i64.lt_u
                    br_if 5 (;@3;)
                    local.get 1
                    call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                    local.get 7
                    local.get 2
                    i64.gt_u
                    br_if 2 (;@6;)
                    br 5 (;@3;)
                  end
                  local.get 7
                  i32.load offset=8
                  local.tee 1
                  i32.eqz
                  br_if 4 (;@3;)
                  local.get 7
                  i64.const 12
                  i64.add
                  local.set 10
                  local.get 1
                  i64.extend_i32_u
                  local.set 12
                  i64.const 0
                  local.set 2
                  loop  ;; label = @8
                    block  ;; label = @9
                      local.get 10
                      local.get 2
                      i64.const 2
                      i64.shl
                      i64.add
                      local.tee 9
                      i32.load
                      local.tee 1
                      i32.const 1
                      i32.eq
                      br_if 0 (;@9;)
                      local.get 1
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 1
                      i64.extend_i32_u
                      local.tee 11
                      i64.const 1
                      i64.add
                      local.get 8
                      i64.lt_u
                      br_if 0 (;@9;)
                      local.get 1
                      call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                      local.get 7
                      local.get 11
                      i64.le_u
                      br_if 0 (;@9;)
                      local.get 9
                      local.get 9
                      i64.load32_u
                      local.tee 11
                      i32.load offset=1
                      i32.store
                      local.get 11
                      local.get 9
                      i64.store32 offset=1
                    end
                    local.get 2
                    i64.const 1
                    i64.add
                    local.tee 2
                    local.get 12
                    i64.ne
                    br_if 0 (;@8;)
                    br 5 (;@3;)
                  end
                end
                local.get 7
                i32.load offset=4
                local.tee 1
                i32.const 1
                i32.eq
                br_if 3 (;@3;)
                local.get 1
                i32.const 1
                i32.and
                i32.eqz
                br_if 3 (;@3;)
                local.get 1
                i64.extend_i32_u
                local.tee 2
                i64.const 1
                i64.add
                local.get 8
                i64.lt_u
                br_if 3 (;@3;)
                local.get 1
                call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
                local.get 7
                local.get 2
                i64.le_u
                br_if 3 (;@3;)
              end
              local.get 7
              i64.const 4
              i64.add
              local.set 2
            end
            local.get 2
            local.get 2
            i64.load32_u
            local.tee 7
            i32.load offset=1
            i32.store
            local.get 7
            local.get 2
            i64.store32 offset=1
            br 1 (;@3;)
          end
          block  ;; label = @4
            local.get 7
            i32.load offset=4
            local.tee 3
            local.get 1
            i32.const 0
            local.get 1
            i32.const 31
            i32.gt_u
            select
            local.tee 1
            i32.sub
            i32.const 128
            i32.lt_u
            br_if 0 (;@4;)
            local.get 7
            local.get 1
            i32.const 127
            i32.add
            local.tee 3
            call $motoko_rts::gc::mark_compact::mark_stack::push_mark_stack::hccc6b644da04914e
          end
          local.get 1
          local.get 3
          i32.ge_u
          br_if 0 (;@3;)
          local.get 7
          i64.const 8
          i64.add
          local.set 10
          local.get 1
          i64.extend_i32_u
          local.set 2
          loop  ;; label = @4
            block  ;; label = @5
              local.get 10
              local.get 2
              i64.const 2
              i64.shl
              i64.add
              local.tee 9
              i32.load
              local.tee 1
              i32.const 1
              i32.eq
              br_if 0 (;@5;)
              local.get 1
              i32.const 1
              i32.and
              i32.eqz
              br_if 0 (;@5;)
              local.get 1
              i64.extend_i32_u
              local.tee 11
              i64.const 1
              i64.add
              local.get 8
              i64.lt_u
              br_if 0 (;@5;)
              local.get 1
              call $motoko_rts::gc::mark_compact::mark_object::h228f774aea7978ca
              local.get 7
              local.get 11
              i64.le_u
              br_if 0 (;@5;)
              local.get 9
              local.get 9
              i64.load32_u
              local.tee 11
              i32.load offset=1
              i32.store
              local.get 11
              local.get 9
              i64.store32 offset=1
            end
            local.get 3
            local.get 2
            i64.const 1
            i64.add
            local.tee 2
            i32.wrap_i64
            i32.ne
            br_if 0 (;@4;)
          end
        end
        global.get $__memory_base
        local.tee 7
        i64.const 3912
        i64.add
        i64.load
        local.tee 2
        local.get 7
        i64.const 3896
        i64.add
        i64.load
        i64.ne
        br_if 0 (;@2;)
      end
    end
    i64.const 0
    local.set 2
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3880
      i64.add
      i64.load
      local.tee 7
      i64.const -4
      i64.add
      i32.load
      local.tee 1
      i32.const 8
      i32.lt_u
      br_if 0 (;@1;)
      local.get 7
      i64.load align=1
      local.set 2
    end
    local.get 0
    local.get 2
    i64.store offset=8
    local.get 0
    local.get 2
    i64.clz
    i64.store32 offset=24
    local.get 0
    local.get 7
    global.get $__memory_base
    i64.const 3872
    i64.add
    i64.load
    i64.sub
    i32.wrap_i64
    local.tee 3
    i32.const 3
    i32.shl
    i32.store offset=20
    local.get 0
    local.get 1
    local.get 3
    i32.add
    i32.const 3
    i32.shl
    i32.store offset=16
    local.get 5
    local.set 13
    block  ;; label = @1
      local.get 0
      i64.const 8
      i64.add
      call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
      local.tee 1
      i32.const -1
      i32.eq
      br_if 0 (;@1;)
      local.get 5
      i64.extend_i32_u
      local.set 8
      local.get 5
      local.set 13
      loop  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.const 3
          i32.shl
          local.tee 6
          i64.extend_i32_u
          local.tee 7
          i32.load
          local.tee 1
          i32.const 1
          i32.and
          br_if 0 (;@3;)
          local.get 13
          i32.const -1
          i32.add
          local.set 3
          loop  ;; label = @4
            local.get 1
            i64.extend_i32_u
            local.tee 2
            i32.load
            local.set 1
            local.get 2
            local.get 3
            i32.store
            local.get 1
            i32.const 1
            i32.and
            i32.eqz
            br_if 0 (;@4;)
          end
        end
        local.get 7
        local.get 1
        i32.store
        local.get 13
        i64.extend_i32_u
        local.set 2
        local.get 7
        call $motoko_rts::types::block_size::h0bf0d784741e3e30
        local.set 1
        block  ;; label = @3
          block  ;; label = @4
            local.get 13
            local.get 6
            i32.ne
            br_if 0 (;@4;)
            local.get 1
            i32.const 3
            i32.shl
            local.set 3
            br 1 (;@3;)
          end
          local.get 2
          local.get 7
          local.get 1
          i32.const 3
          i32.shl
          local.tee 3
          i64.extend_i32_u
          memory.copy
        end
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    local.get 2
                                    i32.load
                                    local.tee 1
                                    i32.const -1
                                    i32.add
                                    br_table 12 (;@4;) 0 (;@16;) 3 (;@13;) 0 (;@16;) 11 (;@5;) 0 (;@16;) 13 (;@3;) 0 (;@16;) 8 (;@8;) 0 (;@16;) 7 (;@9;) 0 (;@16;) 6 (;@10;) 0 (;@16;) 5 (;@11;) 0 (;@16;) 13 (;@3;) 0 (;@16;) 1 (;@15;) 0 (;@16;) 13 (;@3;) 0 (;@16;) 13 (;@3;) 0 (;@16;) 4 (;@12;) 0 (;@16;) 2 (;@14;) 0 (;@16;) 1 (;@15;) 0 (;@16;) 1 (;@15;) 0 (;@16;)
                                  end
                                  local.get 1
                                  i32.const 31
                                  i32.gt_u
                                  br_if 10 (;@5;)
                                end
                                global.get $__memory_base
                                i64.const 1902
                                i64.add
                                i64.const 42
                                call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                unreachable
                              end
                              global.get $__memory_base
                              i64.const 1944
                              i64.add
                              i64.const 51
                              call $motoko_rts::rts_trap_with::h000639acaf03dda1
                              unreachable
                            end
                            local.get 2
                            i32.load offset=4
                            local.tee 1
                            local.get 13
                            i32.lt_u
                            br_if 9 (;@3;)
                            local.get 1
                            i32.const 1
                            i32.eq
                            br_if 9 (;@3;)
                            local.get 1
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 9 (;@3;)
                            local.get 1
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.tee 7
                            local.get 8
                            i64.lt_u
                            br_if 9 (;@3;)
                            br 5 (;@7;)
                          end
                          block  ;; label = @12
                            local.get 2
                            i32.load offset=8
                            local.tee 1
                            local.get 13
                            i32.lt_u
                            br_if 0 (;@12;)
                            local.get 1
                            i32.const 1
                            i32.eq
                            br_if 0 (;@12;)
                            local.get 1
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 1
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.tee 7
                            local.get 8
                            i64.lt_u
                            br_if 0 (;@12;)
                            local.get 2
                            i64.const 8
                            i64.add
                            local.tee 9
                            local.get 7
                            i32.load
                            i32.store
                            local.get 7
                            local.get 9
                            i64.store32
                          end
                          local.get 2
                          i32.load offset=12
                          local.tee 1
                          local.get 13
                          i32.lt_u
                          br_if 8 (;@3;)
                          local.get 1
                          i32.const 1
                          i32.eq
                          br_if 8 (;@3;)
                          local.get 1
                          i32.const 1
                          i32.and
                          i32.eqz
                          br_if 8 (;@3;)
                          local.get 1
                          i64.extend_i32_u
                          i64.const 1
                          i64.add
                          local.tee 7
                          local.get 8
                          i64.lt_u
                          br_if 8 (;@3;)
                          local.get 2
                          i64.const 12
                          i64.add
                          local.set 2
                          br 5 (;@6;)
                        end
                        local.get 2
                        i32.load offset=8
                        local.tee 1
                        local.get 13
                        i32.lt_u
                        br_if 7 (;@3;)
                        local.get 1
                        i32.const 1
                        i32.eq
                        br_if 7 (;@3;)
                        local.get 1
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 7 (;@3;)
                        local.get 1
                        i64.extend_i32_u
                        i64.const 1
                        i64.add
                        local.tee 7
                        local.get 8
                        i64.lt_u
                        br_if 7 (;@3;)
                        local.get 2
                        i64.const 8
                        i64.add
                        local.set 2
                        br 4 (;@6;)
                      end
                      local.get 2
                      i32.load offset=4
                      local.tee 1
                      local.get 13
                      i32.lt_u
                      br_if 6 (;@3;)
                      local.get 1
                      i32.const 1
                      i32.eq
                      br_if 6 (;@3;)
                      local.get 1
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 6 (;@3;)
                      local.get 1
                      i64.extend_i32_u
                      i64.const 1
                      i64.add
                      local.tee 7
                      local.get 8
                      i64.ge_u
                      br_if 2 (;@7;)
                      br 6 (;@3;)
                    end
                    local.get 2
                    i32.load offset=8
                    local.tee 1
                    i32.eqz
                    br_if 5 (;@3;)
                    local.get 2
                    i64.const 12
                    i64.add
                    local.set 11
                    local.get 1
                    i64.extend_i32_u
                    local.set 10
                    i64.const 0
                    local.set 2
                    loop  ;; label = @9
                      block  ;; label = @10
                        local.get 11
                        local.get 2
                        i64.const 2
                        i64.shl
                        i64.add
                        local.tee 7
                        i32.load
                        local.tee 1
                        local.get 13
                        i32.lt_u
                        br_if 0 (;@10;)
                        local.get 1
                        i32.const 1
                        i32.eq
                        br_if 0 (;@10;)
                        local.get 1
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 0 (;@10;)
                        local.get 1
                        i64.extend_i32_u
                        i64.const 1
                        i64.add
                        local.tee 9
                        local.get 8
                        i64.lt_u
                        br_if 0 (;@10;)
                        local.get 7
                        local.get 9
                        i32.load
                        i32.store
                        local.get 9
                        local.get 7
                        i64.store32
                      end
                      local.get 2
                      i64.const 1
                      i64.add
                      local.tee 2
                      local.get 10
                      i64.ne
                      br_if 0 (;@9;)
                      br 6 (;@3;)
                    end
                  end
                  local.get 2
                  i32.load offset=4
                  local.tee 1
                  local.get 13
                  i32.lt_u
                  br_if 4 (;@3;)
                  local.get 1
                  i32.const 1
                  i32.eq
                  br_if 4 (;@3;)
                  local.get 1
                  i32.const 1
                  i32.and
                  i32.eqz
                  br_if 4 (;@3;)
                  local.get 1
                  i64.extend_i32_u
                  i64.const 1
                  i64.add
                  local.tee 7
                  local.get 8
                  i64.lt_u
                  br_if 4 (;@3;)
                end
                local.get 2
                i64.const 4
                i64.add
                local.set 2
              end
              local.get 2
              local.get 7
              i32.load
              i32.store
              local.get 7
              local.get 2
              i64.store32
              br 2 (;@3;)
            end
            local.get 1
            i32.const 0
            local.get 1
            i32.const 31
            i32.gt_u
            select
            local.tee 1
            local.get 2
            i32.load offset=4
            local.tee 6
            i32.ge_u
            br_if 1 (;@3;)
            local.get 2
            i64.const 8
            i64.add
            local.set 11
            local.get 6
            i64.extend_i32_u
            local.set 10
            local.get 1
            i64.extend_i32_u
            local.set 2
            loop  ;; label = @5
              block  ;; label = @6
                local.get 11
                local.get 2
                i64.const 2
                i64.shl
                i64.add
                local.tee 7
                i32.load
                local.tee 1
                local.get 13
                i32.lt_u
                br_if 0 (;@6;)
                local.get 1
                i32.const 1
                i32.eq
                br_if 0 (;@6;)
                local.get 1
                i32.const 1
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                local.get 1
                i64.extend_i32_u
                i64.const 1
                i64.add
                local.tee 9
                local.get 8
                i64.lt_u
                br_if 0 (;@6;)
                local.get 7
                local.get 9
                i32.load
                i32.store
                local.get 9
                local.get 7
                i64.store32
              end
              local.get 2
              i64.const 1
              i64.add
              local.tee 2
              local.get 10
              i64.ne
              br_if 0 (;@5;)
              br 2 (;@3;)
            end
          end
          local.get 2
          i32.load offset=4
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          i64.const 12
          i64.add
          local.set 11
          local.get 1
          i64.extend_i32_u
          local.set 10
          i64.const 0
          local.set 2
          loop  ;; label = @4
            block  ;; label = @5
              local.get 11
              local.get 2
              i64.const 2
              i64.shl
              i64.add
              local.tee 7
              i32.load
              local.tee 1
              local.get 13
              i32.lt_u
              br_if 0 (;@5;)
              local.get 1
              i32.const 1
              i32.eq
              br_if 0 (;@5;)
              local.get 1
              i32.const 1
              i32.and
              i32.eqz
              br_if 0 (;@5;)
              local.get 1
              i64.extend_i32_u
              i64.const 1
              i64.add
              local.tee 9
              local.get 8
              i64.lt_u
              br_if 0 (;@5;)
              local.get 7
              local.get 9
              i32.load
              i32.store
              local.get 9
              local.get 7
              i64.store32
            end
            local.get 2
            i64.const 1
            i64.add
            local.tee 2
            local.get 10
            i64.ne
            br_if 0 (;@4;)
          end
        end
        local.get 3
        local.get 13
        i32.add
        local.set 13
        local.get 0
        i64.const 8
        i64.add
        call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
        local.tee 1
        i32.const -1
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 4
      local.get 13
      i32.sub
      local.set 6
    end
    global.get $__memory_base
    local.tee 2
    i64.const 3928
    i64.add
    local.get 13
    i32.store
    local.get 2
    i64.const 3888
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3896
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3912
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3904
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3880
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3872
    i64.add
    i64.const 0
    i64.store
    local.get 2
    i64.const 3932
    i64.add
    local.get 13
    i32.store
    local.get 2
    i64.const 3920
    i64.add
    local.tee 7
    local.get 7
    i64.load
    local.get 6
    i64.extend_i32_u
    i64.add
    i64.store
    local.get 2
    i64.const 3796
    i64.add
    local.tee 2
    local.get 2
    i32.load
    local.tee 1
    local.get 13
    local.get 5
    i32.sub
    local.tee 3
    local.get 1
    local.get 3
    i32.gt_u
    select
    i32.store
    local.get 0
    i64.const 32
    i64.add
    global.set $__stack_pointer)
  (func $compacting_gc (type 10)
    call $motoko_rts::gc::mark_compact::compacting_gc::hedb61db8ff561f85)
  (func $schedule_compacting_gc (type 10)
    (local i64 i32 i64 i64)
    block  ;; label = @1
      global.get $__memory_base
      local.tee 0
      i64.const 3932
      i64.add
      i32.load
      local.tee 1
      f64.convert_i32_u
      f64.const 0x1.8p+0 (;=1.5;)
      f64.mul
      i64.trunc_sat_f64_u
      local.tee 2
      local.get 1
      i64.extend_i32_u
      i64.const 7784628224
      i64.add
      i64.const 1
      i64.shr_u
      local.tee 3
      local.get 2
      local.get 3
      i64.lt_u
      select
      local.get 0
      i64.const 3928
      i64.add
      i64.load32_u
      i64.gt_u
      br_if 0 (;@1;)
      call $motoko_rts::gc::mark_compact::compacting_gc::hedb61db8ff561f85
    end)
  (func $motoko_rts::gc::generational::generational_gc::h7a9385a87cee170d (type 11) (param i64)
    (local i64 i32 i64 i64 i32 i64 i64 i32 i32 i64 i64 i64 i32 f64)
    global.get $__stack_pointer
    i64.const 192
    i64.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    call $motoko_rts::gc::generational::get_limits::ha6736df7ee81ff11
    call $get_static_roots
    local.set 2
    local.get 1
    i64.const 40
    i64.add
    local.tee 3
    call $motoko_rts::gc::generational::get_limits::ha6736df7ee81ff11
    local.get 1
    local.get 2
    i32.store offset=32
    local.get 1
    global.get $__memory_base
    local.tee 4
    i64.const 3784
    i64.add
    i64.store offset=24
    local.get 1
    local.get 0
    i64.store offset=64
    local.get 3
    call $motoko_rts::gc::generational::decide_strategy::h7620e853babf9e48
    local.set 2
    local.get 1
    i64.const 72
    i64.add
    local.get 1
    i64.const 24
    i64.add
    i64.const 48
    memory.copy
    local.get 1
    local.get 2
    i32.const 253
    i32.and
    local.tee 5
    i32.store8 offset=128
    local.get 1
    i64.const 104
    i64.add
    i64.load
    local.get 1
    i64.load offset=88
    local.get 1
    i64.const 96
    i64.add
    i64.load
    i64.const -64
    i64.and
    local.get 5
    select
    local.tee 0
    i64.sub
    i32.wrap_i64
    local.get 0
    i32.wrap_i64
    i32.const 3
    i32.shr_u
    call $motoko_rts::gc::mark_compact::bitmap::alloc_bitmap::h15bb744d451f0bbd
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 4
              i64.const 3800
              i64.add
              i64.load
              i64.eqz
              i32.eqz
              br_if 0 (;@5;)
              global.get $__memory_base
              local.tee 0
              i64.const 3808
              i64.add
              i32.const 512
              call $motoko_rts::memory::alloc_blob::h5034ff391397ca60
              i64.extend_i32_u
              local.tee 3
              i64.const 9
              i64.add
              local.tee 4
              i64.store
              local.get 0
              i64.const 3800
              i64.add
              local.get 3
              i64.const 1
              i64.add
              i64.store
              local.get 0
              i64.const 3824
              i64.add
              local.get 4
              i64.store
              local.get 0
              i64.const 3816
              i64.add
              local.get 3
              i64.const 521
              i64.add
              i64.store
              i64.const 0
              local.set 0
              local.get 1
              i64.const 0
              i64.store offset=120
              block  ;; label = @6
                local.get 1
                i64.load32_u offset=80
                local.tee 3
                i64.const 5
                i64.add
                i32.load
                local.tee 2
                i32.eqz
                br_if 0 (;@6;)
                local.get 3
                i64.const 1
                i64.add
                i64.const 8
                i64.add
                local.set 6
                local.get 2
                i64.extend_i32_u
                local.set 7
                loop  ;; label = @7
                  local.get 1
                  local.get 0
                  i64.const 3
                  i64.shl
                  i64.const 4294967288
                  i64.and
                  local.get 6
                  i64.add
                  i64.load32_u
                  local.tee 3
                  i32.load offset=1
                  local.tee 2
                  i32.store offset=140
                  local.get 2
                  i32.const 9
                  i32.ne
                  br_if 3 (;@4;)
                  local.get 1
                  i64.load offset=88
                  local.tee 4
                  local.get 3
                  i64.const 1
                  i64.add
                  local.tee 3
                  i64.le_u
                  br_if 4 (;@3;)
                  block  ;; label = @8
                    local.get 3
                    i32.load offset=4
                    local.tee 2
                    i32.const 1
                    i32.eq
                    br_if 0 (;@8;)
                    local.get 2
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 2
                    i64.extend_i32_u
                    i64.const 1
                    i64.add
                    local.get 4
                    local.get 1
                    i64.load offset=96
                    local.get 1
                    i32.load8_u offset=128
                    select
                    i64.lt_u
                    br_if 0 (;@8;)
                    local.get 1
                    i64.const 72
                    i64.add
                    local.get 2
                    call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                  end
                  local.get 0
                  i64.const 1
                  i64.add
                  local.tee 0
                  local.get 7
                  i64.ne
                  br_if 0 (;@7;)
                end
              end
              i32.const 1
              local.set 8
              local.get 1
              i64.load offset=72
              i32.load
              local.tee 2
              i32.const 1
              i32.eq
              br_if 4 (;@1;)
              local.get 2
              i32.const 1
              i32.and
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              i64.extend_i32_u
              i64.const 1
              i64.add
              local.get 1
              i64.const 72
              i64.add
              i64.const 16
              i64.const 24
              local.get 1
              i32.load8_u offset=128
              select
              i64.add
              i64.load
              i64.ge_u
              br_if 3 (;@2;)
              br 4 (;@1;)
            end
            global.get $__memory_base
            local.tee 1
            i64.const 2072
            i64.add
            i64.const 42
            local.get 1
            i64.const 3272
            i64.add
            call $core::panicking::panic::h1108c90903a88b24
            unreachable
          end
          local.get 1
          i64.const 0
          i64.store offset=160
          local.get 1
          i64.const 140
          i64.add
          global.get $__memory_base
          local.tee 0
          i64.const 2376
          i64.add
          local.get 1
          i64.const 144
          i64.add
          local.get 0
          i64.const 3424
          i64.add
          call $core::panicking::assert_failed::ha6cca40bd32447e3
          unreachable
        end
        global.get $__memory_base
        local.tee 1
        i64.const 2380
        i64.add
        i64.const 59
        local.get 1
        i64.const 3448
        i64.add
        call $core::panicking::panic::h1108c90903a88b24
        unreachable
      end
      local.get 1
      i64.const 72
      i64.add
      local.get 2
      call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load8_u offset=128
            br_if 0 (;@4;)
            global.get $__memory_base
            i64.const 3832
            i64.add
            i64.load
            i64.const 0
            i64.eq
            br_if 1 (;@3;)
            global.get $__memory_base
            i64.const 3832
            i64.add
            i64.load offset=8
            local.tee 0
            i64.const 8
            i64.add
            i64.const 4294967295
            i64.and
            local.tee 3
            i32.load
            local.set 2
            i32.const 0
            local.set 8
            local.get 1
            i32.const 0
            i32.store offset=160
            local.get 1
            local.get 0
            i64.store offset=144
            local.get 1
            local.get 3
            i64.const 0
            local.get 2
            select
            i64.store offset=152
            local.get 1
            i64.const 144
            i64.add
            call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
            local.get 1
            i64.load offset=152
            local.tee 0
            i64.eqz
            br_if 0 (;@4;)
            loop  ;; label = @5
              block  ;; label = @6
                local.get 0
                i64.load32_u
                i32.load
                local.tee 2
                i32.const 1
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                local.get 2
                i64.extend_i32_u
                i64.const 1
                i64.add
                local.get 1
                i64.load offset=96
                i64.lt_u
                br_if 0 (;@6;)
                local.get 1
                i64.const 72
                i64.add
                local.get 2
                call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
              end
              local.get 1
              i64.const 0
              local.get 0
              i64.load offset=8
              local.tee 0
              i64.const 8
              i64.add
              local.get 0
              i64.eqz
              select
              i64.store offset=152
              local.get 1
              i64.const 144
              i64.add
              call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
              local.get 1
              i64.load offset=152
              local.tee 0
              i64.eqz
              i32.eqz
              br_if 0 (;@5;)
            end
            local.get 1
            i32.load8_u offset=128
            local.set 8
          end
          global.get $__memory_base
          local.set 3
          local.get 1
          i64.load offset=88
          local.set 6
          local.get 1
          i64.load offset=96
          local.set 4
          block  ;; label = @4
            local.get 3
            i64.const 3824
            i64.add
            i64.load
            local.tee 0
            local.get 3
            i64.const 3808
            i64.add
            i64.load
            i64.eq
            br_if 0 (;@4;)
            local.get 8
            local.set 9
            loop  ;; label = @5
              global.get $__memory_base
              i64.const 3824
              i64.add
              local.get 0
              i64.const -8
              i64.add
              local.tee 0
              i64.store
              local.get 6
              local.get 4
              local.get 9
              i32.const 255
              i32.and
              select
              local.set 10
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          block  ;; label = @20
                                            block  ;; label = @21
                                              block  ;; label = @22
                                                local.get 0
                                                i64.load
                                                local.tee 0
                                                i32.load
                                                local.tee 2
                                                i32.const -1
                                                i32.add
                                                br_table 0 (;@22;) 1 (;@21;) 4 (;@18;) 1 (;@21;) 15 (;@7;) 1 (;@21;) 16 (;@6;) 1 (;@21;) 9 (;@13;) 1 (;@21;) 8 (;@14;) 1 (;@21;) 7 (;@15;) 1 (;@21;) 6 (;@16;) 1 (;@21;) 16 (;@6;) 1 (;@21;) 2 (;@20;) 1 (;@21;) 16 (;@6;) 1 (;@21;) 16 (;@6;) 1 (;@21;) 5 (;@17;) 1 (;@21;) 3 (;@19;) 1 (;@21;) 2 (;@20;) 1 (;@21;) 2 (;@20;) 1 (;@21;)
                                              end
                                              local.get 0
                                              i32.load offset=4
                                              local.tee 2
                                              i32.eqz
                                              br_if 15 (;@6;)
                                              local.get 0
                                              i64.const 12
                                              i64.add
                                              local.set 11
                                              local.get 2
                                              i64.extend_i32_u
                                              local.set 12
                                              i64.const 0
                                              local.set 7
                                              loop  ;; label = @22
                                                block  ;; label = @23
                                                  local.get 11
                                                  local.get 7
                                                  i64.const 2
                                                  i64.shl
                                                  i64.add
                                                  local.tee 0
                                                  i32.load
                                                  local.tee 2
                                                  i32.const 1
                                                  i32.eq
                                                  br_if 0 (;@23;)
                                                  local.get 2
                                                  i32.const 1
                                                  i32.and
                                                  i32.eqz
                                                  br_if 0 (;@23;)
                                                  local.get 2
                                                  i64.extend_i32_u
                                                  i64.const 1
                                                  i64.add
                                                  local.get 10
                                                  i64.lt_u
                                                  br_if 0 (;@23;)
                                                  local.get 1
                                                  i64.const 72
                                                  i64.add
                                                  local.get 2
                                                  call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                                  local.get 1
                                                  i64.load offset=96
                                                  local.set 4
                                                  local.get 1
                                                  i64.load offset=88
                                                  local.set 6
                                                  block  ;; label = @24
                                                    local.get 1
                                                    i32.load8_u offset=128
                                                    local.tee 8
                                                    i32.eqz
                                                    br_if 0 (;@24;)
                                                    local.get 6
                                                    local.get 0
                                                    i64.gt_u
                                                    br_if 0 (;@24;)
                                                    local.get 4
                                                    local.get 0
                                                    i64.le_u
                                                    br_if 0 (;@24;)
                                                    i32.const 1
                                                    local.set 8
                                                    block  ;; label = @25
                                                      local.get 0
                                                      i32.load
                                                      local.tee 2
                                                      i32.const 1
                                                      i32.and
                                                      i32.eqz
                                                      br_if 0 (;@25;)
                                                      local.get 2
                                                      i64.extend_i32_u
                                                      i64.const 1
                                                      i64.add
                                                      local.get 4
                                                      i64.lt_u
                                                      br_if 0 (;@25;)
                                                      block  ;; label = @26
                                                        global.get $__memory_base
                                                        i64.const 3832
                                                        i64.add
                                                        i64.load
                                                        i64.const 0
                                                        i64.eq
                                                        br_if 0 (;@26;)
                                                        block  ;; label = @27
                                                          global.get $__memory_base
                                                          i64.const 3832
                                                          i64.add
                                                          i64.load offset=8
                                                          local.tee 3
                                                          i32.wrap_i64
                                                          local.get 3
                                                          i32.load offset=4
                                                          i32.const 4
                                                          i32.shr_u
                                                          i32.const -1
                                                          i32.add
                                                          local.get 0
                                                          i32.wrap_i64
                                                          local.tee 2
                                                          i32.const 3
                                                          i32.shr_u
                                                          i32.and
                                                          i32.const 4
                                                          i32.shl
                                                          i32.add
                                                          i32.const 8
                                                          i32.add
                                                          i64.extend_i32_u
                                                          local.tee 3
                                                          i32.load
                                                          local.tee 9
                                                          i32.eqz
                                                          br_if 0 (;@27;)
                                                          local.get 9
                                                          local.get 2
                                                          i32.eq
                                                          br_if 2 (;@25;)
                                                          loop  ;; label = @28
                                                            local.get 3
                                                            i64.load offset=8
                                                            local.tee 0
                                                            i64.const 0
                                                            i64.eq
                                                            br_if 1 (;@27;)
                                                            local.get 0
                                                            i64.const 8
                                                            i64.add
                                                            local.set 3
                                                            local.get 0
                                                            i32.load offset=8
                                                            local.get 2
                                                            i32.eq
                                                            br_if 3 (;@25;)
                                                            br 0 (;@28;)
                                                          end
                                                        end
                                                        global.get $__memory_base
                                                        local.tee 1
                                                        i64.const 2497
                                                        i64.add
                                                        i64.const 111
                                                        local.get 1
                                                        i64.const 3568
                                                        i64.add
                                                        call $core::panicking::panic::h1108c90903a88b24
                                                        unreachable
                                                      end
                                                      global.get $__memory_base
                                                      local.tee 1
                                                      i64.const 148
                                                      i64.add
                                                      i64.const 43
                                                      local.get 1
                                                      i64.const 3544
                                                      i64.add
                                                      call $core::panicking::panic::h1108c90903a88b24
                                                      unreachable
                                                    end
                                                    i32.const 1
                                                    local.set 9
                                                    br 1 (;@23;)
                                                  end
                                                  local.get 8
                                                  local.set 9
                                                end
                                                local.get 7
                                                i64.const 1
                                                i64.add
                                                local.tee 7
                                                local.get 12
                                                i64.ne
                                                br_if 0 (;@22;)
                                                br 16 (;@6;)
                                              end
                                            end
                                            local.get 2
                                            i32.const 31
                                            i32.gt_u
                                            br_if 13 (;@7;)
                                          end
                                          global.get $__memory_base
                                          i64.const 1902
                                          i64.add
                                          i64.const 42
                                          call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                          unreachable
                                        end
                                        global.get $__memory_base
                                        i64.const 1944
                                        i64.add
                                        i64.const 51
                                        call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                        unreachable
                                      end
                                      local.get 0
                                      i32.load offset=4
                                      local.tee 2
                                      i32.const 1
                                      i32.eq
                                      br_if 11 (;@6;)
                                      local.get 2
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      br_if 11 (;@6;)
                                      local.get 2
                                      i64.extend_i32_u
                                      i64.const 1
                                      i64.add
                                      local.get 10
                                      i64.lt_u
                                      br_if 11 (;@6;)
                                      local.get 1
                                      i64.const 72
                                      i64.add
                                      local.get 2
                                      call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                      local.get 1
                                      i64.load offset=96
                                      local.set 4
                                      local.get 1
                                      i64.load offset=88
                                      local.set 6
                                      local.get 1
                                      i32.load8_u offset=128
                                      local.tee 8
                                      i32.eqz
                                      br_if 5 (;@12;)
                                      local.get 6
                                      local.get 0
                                      i64.const 4
                                      i64.add
                                      local.tee 0
                                      i64.gt_u
                                      br_if 5 (;@12;)
                                      local.get 4
                                      local.get 0
                                      i64.le_u
                                      br_if 5 (;@12;)
                                      i32.const 1
                                      local.set 8
                                      local.get 0
                                      i32.load
                                      local.tee 2
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      br_if 9 (;@8;)
                                      local.get 2
                                      i64.extend_i32_u
                                      i64.const 1
                                      i64.add
                                      local.get 4
                                      i64.lt_u
                                      br_if 9 (;@8;)
                                      block  ;; label = @18
                                        global.get $__memory_base
                                        i64.const 3832
                                        i64.add
                                        i64.load
                                        i64.const 0
                                        i64.eq
                                        br_if 0 (;@18;)
                                        block  ;; label = @19
                                          global.get $__memory_base
                                          i64.const 3832
                                          i64.add
                                          i64.load offset=8
                                          local.tee 3
                                          i32.wrap_i64
                                          local.get 3
                                          i32.load offset=4
                                          i32.const 4
                                          i32.shr_u
                                          i32.const -1
                                          i32.add
                                          local.get 0
                                          i32.wrap_i64
                                          local.tee 2
                                          i32.const 3
                                          i32.shr_u
                                          i32.and
                                          i32.const 4
                                          i32.shl
                                          i32.add
                                          i32.const 8
                                          i32.add
                                          i64.extend_i32_u
                                          local.tee 3
                                          i32.load
                                          local.tee 9
                                          i32.eqz
                                          br_if 0 (;@19;)
                                          local.get 9
                                          local.get 2
                                          i32.eq
                                          br_if 11 (;@8;)
                                          loop  ;; label = @20
                                            local.get 3
                                            i64.load offset=8
                                            local.tee 0
                                            i64.const 0
                                            i64.eq
                                            br_if 1 (;@19;)
                                            local.get 0
                                            i64.const 8
                                            i64.add
                                            local.set 3
                                            local.get 0
                                            i32.load offset=8
                                            local.get 2
                                            i32.eq
                                            br_if 12 (;@8;)
                                            br 0 (;@20;)
                                          end
                                        end
                                        global.get $__memory_base
                                        local.tee 1
                                        i64.const 2497
                                        i64.add
                                        i64.const 111
                                        local.get 1
                                        i64.const 3568
                                        i64.add
                                        call $core::panicking::panic::h1108c90903a88b24
                                        unreachable
                                      end
                                      global.get $__memory_base
                                      local.tee 1
                                      i64.const 148
                                      i64.add
                                      i64.const 43
                                      local.get 1
                                      i64.const 3544
                                      i64.add
                                      call $core::panicking::panic::h1108c90903a88b24
                                      unreachable
                                    end
                                    block  ;; label = @17
                                      local.get 0
                                      i32.load offset=8
                                      local.tee 2
                                      i32.const 1
                                      i32.eq
                                      br_if 0 (;@17;)
                                      local.get 2
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 2
                                      i64.extend_i32_u
                                      i64.const 1
                                      i64.add
                                      local.get 10
                                      i64.lt_u
                                      br_if 0 (;@17;)
                                      local.get 1
                                      i64.const 72
                                      i64.add
                                      local.get 2
                                      call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                      local.get 1
                                      i64.load offset=96
                                      local.set 4
                                      local.get 1
                                      i64.load offset=88
                                      local.set 6
                                      block  ;; label = @18
                                        local.get 1
                                        i32.load8_u offset=128
                                        local.tee 8
                                        i32.eqz
                                        br_if 0 (;@18;)
                                        local.get 6
                                        local.get 0
                                        i64.const 8
                                        i64.add
                                        local.tee 3
                                        i64.gt_u
                                        br_if 0 (;@18;)
                                        local.get 4
                                        local.get 3
                                        i64.le_u
                                        br_if 0 (;@18;)
                                        i32.const 1
                                        local.set 8
                                        block  ;; label = @19
                                          local.get 3
                                          i32.load
                                          local.tee 2
                                          i32.const 1
                                          i32.and
                                          i32.eqz
                                          br_if 0 (;@19;)
                                          local.get 2
                                          i64.extend_i32_u
                                          i64.const 1
                                          i64.add
                                          local.get 4
                                          i64.lt_u
                                          br_if 0 (;@19;)
                                          block  ;; label = @20
                                            global.get $__memory_base
                                            i64.const 3832
                                            i64.add
                                            i64.load
                                            i64.const 0
                                            i64.eq
                                            br_if 0 (;@20;)
                                            block  ;; label = @21
                                              global.get $__memory_base
                                              i64.const 3832
                                              i64.add
                                              i64.load offset=8
                                              local.tee 7
                                              i32.wrap_i64
                                              local.get 7
                                              i32.load offset=4
                                              i32.const 4
                                              i32.shr_u
                                              i32.const -1
                                              i32.add
                                              local.get 3
                                              i32.wrap_i64
                                              local.tee 2
                                              i32.const 3
                                              i32.shr_u
                                              i32.and
                                              i32.const 4
                                              i32.shl
                                              i32.add
                                              i32.const 8
                                              i32.add
                                              i64.extend_i32_u
                                              local.tee 7
                                              i32.load
                                              local.tee 9
                                              i32.eqz
                                              br_if 0 (;@21;)
                                              local.get 9
                                              local.get 2
                                              i32.eq
                                              br_if 2 (;@19;)
                                              loop  ;; label = @22
                                                local.get 7
                                                i64.load offset=8
                                                local.tee 3
                                                i64.const 0
                                                i64.eq
                                                br_if 1 (;@21;)
                                                local.get 3
                                                i64.const 8
                                                i64.add
                                                local.set 7
                                                local.get 3
                                                i32.load offset=8
                                                local.get 2
                                                i32.eq
                                                br_if 3 (;@19;)
                                                br 0 (;@22;)
                                              end
                                            end
                                            global.get $__memory_base
                                            local.tee 1
                                            i64.const 2497
                                            i64.add
                                            i64.const 111
                                            local.get 1
                                            i64.const 3568
                                            i64.add
                                            call $core::panicking::panic::h1108c90903a88b24
                                            unreachable
                                          end
                                          global.get $__memory_base
                                          local.tee 1
                                          i64.const 148
                                          i64.add
                                          i64.const 43
                                          local.get 1
                                          i64.const 3544
                                          i64.add
                                          call $core::panicking::panic::h1108c90903a88b24
                                          unreachable
                                        end
                                        i32.const 1
                                        local.set 9
                                        br 1 (;@17;)
                                      end
                                      local.get 8
                                      local.set 9
                                    end
                                    local.get 0
                                    i32.load offset=12
                                    local.tee 2
                                    i32.const 1
                                    i32.eq
                                    br_if 10 (;@6;)
                                    local.get 2
                                    i32.const 1
                                    i32.and
                                    i32.eqz
                                    br_if 10 (;@6;)
                                    local.get 2
                                    i64.extend_i32_u
                                    i64.const 1
                                    i64.add
                                    local.get 10
                                    i64.lt_u
                                    br_if 10 (;@6;)
                                    local.get 1
                                    i64.const 72
                                    i64.add
                                    local.get 2
                                    call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                    local.get 1
                                    i64.load offset=96
                                    local.set 4
                                    local.get 1
                                    i64.load offset=88
                                    local.set 6
                                    local.get 1
                                    i32.load8_u offset=128
                                    local.tee 8
                                    i32.eqz
                                    br_if 4 (;@12;)
                                    local.get 6
                                    local.get 0
                                    i64.const 12
                                    i64.add
                                    local.tee 0
                                    i64.gt_u
                                    br_if 4 (;@12;)
                                    local.get 4
                                    local.get 0
                                    i64.le_u
                                    br_if 4 (;@12;)
                                    i32.const 1
                                    local.set 8
                                    local.get 0
                                    i32.load
                                    local.tee 2
                                    i32.const 1
                                    i32.and
                                    i32.eqz
                                    br_if 8 (;@8;)
                                    local.get 2
                                    i64.extend_i32_u
                                    i64.const 1
                                    i64.add
                                    local.get 4
                                    i64.lt_u
                                    br_if 8 (;@8;)
                                    block  ;; label = @17
                                      global.get $__memory_base
                                      i64.const 3832
                                      i64.add
                                      i64.load
                                      i64.const 0
                                      i64.eq
                                      br_if 0 (;@17;)
                                      block  ;; label = @18
                                        global.get $__memory_base
                                        i64.const 3832
                                        i64.add
                                        i64.load offset=8
                                        local.tee 3
                                        i32.wrap_i64
                                        local.get 3
                                        i32.load offset=4
                                        i32.const 4
                                        i32.shr_u
                                        i32.const -1
                                        i32.add
                                        local.get 0
                                        i32.wrap_i64
                                        local.tee 2
                                        i32.const 3
                                        i32.shr_u
                                        i32.and
                                        i32.const 4
                                        i32.shl
                                        i32.add
                                        i32.const 8
                                        i32.add
                                        i64.extend_i32_u
                                        local.tee 3
                                        i32.load
                                        local.tee 9
                                        i32.eqz
                                        br_if 0 (;@18;)
                                        local.get 9
                                        local.get 2
                                        i32.eq
                                        br_if 10 (;@8;)
                                        loop  ;; label = @19
                                          local.get 3
                                          i64.load offset=8
                                          local.tee 0
                                          i64.const 0
                                          i64.eq
                                          br_if 1 (;@18;)
                                          local.get 0
                                          i64.const 8
                                          i64.add
                                          local.set 3
                                          local.get 0
                                          i32.load offset=8
                                          local.get 2
                                          i32.eq
                                          br_if 11 (;@8;)
                                          br 0 (;@19;)
                                        end
                                      end
                                      global.get $__memory_base
                                      local.tee 1
                                      i64.const 2497
                                      i64.add
                                      i64.const 111
                                      local.get 1
                                      i64.const 3568
                                      i64.add
                                      call $core::panicking::panic::h1108c90903a88b24
                                      unreachable
                                    end
                                    global.get $__memory_base
                                    local.tee 1
                                    i64.const 148
                                    i64.add
                                    i64.const 43
                                    local.get 1
                                    i64.const 3544
                                    i64.add
                                    call $core::panicking::panic::h1108c90903a88b24
                                    unreachable
                                  end
                                  local.get 0
                                  i32.load offset=8
                                  local.tee 2
                                  i32.const 1
                                  i32.eq
                                  br_if 9 (;@6;)
                                  local.get 2
                                  i32.const 1
                                  i32.and
                                  i32.eqz
                                  br_if 9 (;@6;)
                                  local.get 2
                                  i64.extend_i32_u
                                  i64.const 1
                                  i64.add
                                  local.get 10
                                  i64.lt_u
                                  br_if 9 (;@6;)
                                  local.get 1
                                  i64.const 72
                                  i64.add
                                  local.get 2
                                  call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                  local.get 1
                                  i64.load offset=96
                                  local.set 4
                                  local.get 1
                                  i64.load offset=88
                                  local.set 6
                                  local.get 1
                                  i32.load8_u offset=128
                                  local.tee 8
                                  i32.eqz
                                  br_if 3 (;@12;)
                                  local.get 6
                                  local.get 0
                                  i64.const 8
                                  i64.add
                                  local.tee 0
                                  i64.gt_u
                                  br_if 3 (;@12;)
                                  local.get 4
                                  local.get 0
                                  i64.le_u
                                  br_if 3 (;@12;)
                                  i32.const 1
                                  local.set 8
                                  local.get 0
                                  i32.load
                                  local.tee 2
                                  i32.const 1
                                  i32.and
                                  i32.eqz
                                  br_if 7 (;@8;)
                                  local.get 2
                                  i64.extend_i32_u
                                  i64.const 1
                                  i64.add
                                  local.get 4
                                  i64.lt_u
                                  br_if 7 (;@8;)
                                  block  ;; label = @16
                                    global.get $__memory_base
                                    i64.const 3832
                                    i64.add
                                    i64.load
                                    i64.const 0
                                    i64.eq
                                    br_if 0 (;@16;)
                                    block  ;; label = @17
                                      global.get $__memory_base
                                      i64.const 3832
                                      i64.add
                                      i64.load offset=8
                                      local.tee 3
                                      i32.wrap_i64
                                      local.get 3
                                      i32.load offset=4
                                      i32.const 4
                                      i32.shr_u
                                      i32.const -1
                                      i32.add
                                      local.get 0
                                      i32.wrap_i64
                                      local.tee 2
                                      i32.const 3
                                      i32.shr_u
                                      i32.and
                                      i32.const 4
                                      i32.shl
                                      i32.add
                                      i32.const 8
                                      i32.add
                                      i64.extend_i32_u
                                      local.tee 3
                                      i32.load
                                      local.tee 9
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 9
                                      local.get 2
                                      i32.eq
                                      br_if 9 (;@8;)
                                      loop  ;; label = @18
                                        local.get 3
                                        i64.load offset=8
                                        local.tee 0
                                        i64.const 0
                                        i64.eq
                                        br_if 1 (;@17;)
                                        local.get 0
                                        i64.const 8
                                        i64.add
                                        local.set 3
                                        local.get 0
                                        i32.load offset=8
                                        local.get 2
                                        i32.eq
                                        br_if 10 (;@8;)
                                        br 0 (;@18;)
                                      end
                                    end
                                    global.get $__memory_base
                                    local.tee 1
                                    i64.const 2497
                                    i64.add
                                    i64.const 111
                                    local.get 1
                                    i64.const 3568
                                    i64.add
                                    call $core::panicking::panic::h1108c90903a88b24
                                    unreachable
                                  end
                                  global.get $__memory_base
                                  local.tee 1
                                  i64.const 148
                                  i64.add
                                  i64.const 43
                                  local.get 1
                                  i64.const 3544
                                  i64.add
                                  call $core::panicking::panic::h1108c90903a88b24
                                  unreachable
                                end
                                local.get 0
                                i32.load offset=4
                                local.tee 2
                                i32.const 1
                                i32.eq
                                br_if 8 (;@6;)
                                local.get 2
                                i32.const 1
                                i32.and
                                i32.eqz
                                br_if 8 (;@6;)
                                local.get 2
                                i64.extend_i32_u
                                i64.const 1
                                i64.add
                                local.get 10
                                i64.lt_u
                                br_if 8 (;@6;)
                                local.get 1
                                i64.const 72
                                i64.add
                                local.get 2
                                call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                local.get 1
                                i64.load offset=96
                                local.set 4
                                local.get 1
                                i64.load offset=88
                                local.set 6
                                local.get 1
                                i32.load8_u offset=128
                                local.tee 8
                                i32.eqz
                                br_if 2 (;@12;)
                                local.get 6
                                local.get 0
                                i64.const 4
                                i64.add
                                local.tee 0
                                i64.gt_u
                                br_if 2 (;@12;)
                                local.get 4
                                local.get 0
                                i64.le_u
                                br_if 2 (;@12;)
                                i32.const 1
                                local.set 8
                                local.get 0
                                i32.load
                                local.tee 2
                                i32.const 1
                                i32.and
                                i32.eqz
                                br_if 6 (;@8;)
                                local.get 2
                                i64.extend_i32_u
                                i64.const 1
                                i64.add
                                local.get 4
                                i64.lt_u
                                br_if 6 (;@8;)
                                block  ;; label = @15
                                  global.get $__memory_base
                                  i64.const 3832
                                  i64.add
                                  i64.load
                                  i64.const 0
                                  i64.eq
                                  br_if 0 (;@15;)
                                  block  ;; label = @16
                                    global.get $__memory_base
                                    i64.const 3832
                                    i64.add
                                    i64.load offset=8
                                    local.tee 3
                                    i32.wrap_i64
                                    local.get 3
                                    i32.load offset=4
                                    i32.const 4
                                    i32.shr_u
                                    i32.const -1
                                    i32.add
                                    local.get 0
                                    i32.wrap_i64
                                    local.tee 2
                                    i32.const 3
                                    i32.shr_u
                                    i32.and
                                    i32.const 4
                                    i32.shl
                                    i32.add
                                    i32.const 8
                                    i32.add
                                    i64.extend_i32_u
                                    local.tee 3
                                    i32.load
                                    local.tee 9
                                    i32.eqz
                                    br_if 0 (;@16;)
                                    local.get 9
                                    local.get 2
                                    i32.eq
                                    br_if 8 (;@8;)
                                    loop  ;; label = @17
                                      local.get 3
                                      i64.load offset=8
                                      local.tee 0
                                      i64.const 0
                                      i64.eq
                                      br_if 1 (;@16;)
                                      local.get 0
                                      i64.const 8
                                      i64.add
                                      local.set 3
                                      local.get 0
                                      i32.load offset=8
                                      local.get 2
                                      i32.eq
                                      br_if 9 (;@8;)
                                      br 0 (;@17;)
                                    end
                                  end
                                  global.get $__memory_base
                                  local.tee 1
                                  i64.const 2497
                                  i64.add
                                  i64.const 111
                                  local.get 1
                                  i64.const 3568
                                  i64.add
                                  call $core::panicking::panic::h1108c90903a88b24
                                  unreachable
                                end
                                global.get $__memory_base
                                local.tee 1
                                i64.const 148
                                i64.add
                                i64.const 43
                                local.get 1
                                i64.const 3544
                                i64.add
                                call $core::panicking::panic::h1108c90903a88b24
                                unreachable
                              end
                              local.get 0
                              i32.load offset=8
                              local.tee 2
                              i32.eqz
                              br_if 7 (;@6;)
                              local.get 0
                              i64.const 12
                              i64.add
                              local.set 11
                              local.get 2
                              i64.extend_i32_u
                              local.set 12
                              i64.const 0
                              local.set 7
                              loop  ;; label = @14
                                block  ;; label = @15
                                  local.get 11
                                  local.get 7
                                  i64.const 2
                                  i64.shl
                                  i64.add
                                  local.tee 0
                                  i32.load
                                  local.tee 2
                                  i32.const 1
                                  i32.eq
                                  br_if 0 (;@15;)
                                  local.get 2
                                  i32.const 1
                                  i32.and
                                  i32.eqz
                                  br_if 0 (;@15;)
                                  local.get 2
                                  i64.extend_i32_u
                                  i64.const 1
                                  i64.add
                                  local.get 10
                                  i64.lt_u
                                  br_if 0 (;@15;)
                                  local.get 1
                                  i64.const 72
                                  i64.add
                                  local.get 2
                                  call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                                  local.get 1
                                  i64.load offset=96
                                  local.set 4
                                  local.get 1
                                  i64.load offset=88
                                  local.set 6
                                  block  ;; label = @16
                                    local.get 1
                                    i32.load8_u offset=128
                                    local.tee 8
                                    i32.eqz
                                    br_if 0 (;@16;)
                                    local.get 6
                                    local.get 0
                                    i64.gt_u
                                    br_if 0 (;@16;)
                                    local.get 4
                                    local.get 0
                                    i64.le_u
                                    br_if 0 (;@16;)
                                    i32.const 1
                                    local.set 8
                                    block  ;; label = @17
                                      local.get 0
                                      i32.load
                                      local.tee 2
                                      i32.const 1
                                      i32.and
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      local.get 2
                                      i64.extend_i32_u
                                      i64.const 1
                                      i64.add
                                      local.get 4
                                      i64.lt_u
                                      br_if 0 (;@17;)
                                      block  ;; label = @18
                                        global.get $__memory_base
                                        i64.const 3832
                                        i64.add
                                        i64.load
                                        i64.const 0
                                        i64.eq
                                        br_if 0 (;@18;)
                                        block  ;; label = @19
                                          global.get $__memory_base
                                          i64.const 3832
                                          i64.add
                                          i64.load offset=8
                                          local.tee 3
                                          i32.wrap_i64
                                          local.get 3
                                          i32.load offset=4
                                          i32.const 4
                                          i32.shr_u
                                          i32.const -1
                                          i32.add
                                          local.get 0
                                          i32.wrap_i64
                                          local.tee 2
                                          i32.const 3
                                          i32.shr_u
                                          i32.and
                                          i32.const 4
                                          i32.shl
                                          i32.add
                                          i32.const 8
                                          i32.add
                                          i64.extend_i32_u
                                          local.tee 3
                                          i32.load
                                          local.tee 9
                                          i32.eqz
                                          br_if 0 (;@19;)
                                          local.get 9
                                          local.get 2
                                          i32.eq
                                          br_if 2 (;@17;)
                                          loop  ;; label = @20
                                            local.get 3
                                            i64.load offset=8
                                            local.tee 0
                                            i64.const 0
                                            i64.eq
                                            br_if 1 (;@19;)
                                            local.get 0
                                            i64.const 8
                                            i64.add
                                            local.set 3
                                            local.get 0
                                            i32.load offset=8
                                            local.get 2
                                            i32.eq
                                            br_if 3 (;@17;)
                                            br 0 (;@20;)
                                          end
                                        end
                                        global.get $__memory_base
                                        local.tee 1
                                        i64.const 2497
                                        i64.add
                                        i64.const 111
                                        local.get 1
                                        i64.const 3568
                                        i64.add
                                        call $core::panicking::panic::h1108c90903a88b24
                                        unreachable
                                      end
                                      global.get $__memory_base
                                      local.tee 1
                                      i64.const 148
                                      i64.add
                                      i64.const 43
                                      local.get 1
                                      i64.const 3544
                                      i64.add
                                      call $core::panicking::panic::h1108c90903a88b24
                                      unreachable
                                    end
                                    i32.const 1
                                    local.set 9
                                    br 1 (;@15;)
                                  end
                                  local.get 8
                                  local.set 9
                                end
                                local.get 7
                                i64.const 1
                                i64.add
                                local.tee 7
                                local.get 12
                                i64.ne
                                br_if 0 (;@14;)
                                br 8 (;@6;)
                              end
                            end
                            local.get 0
                            i32.load offset=4
                            local.tee 2
                            i32.const 1
                            i32.eq
                            br_if 6 (;@6;)
                            local.get 2
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 6 (;@6;)
                            local.get 2
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.get 10
                            i64.lt_u
                            br_if 6 (;@6;)
                            local.get 1
                            i64.const 72
                            i64.add
                            local.get 2
                            call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                            local.get 1
                            i64.load offset=96
                            local.set 4
                            local.get 1
                            i64.load offset=88
                            local.set 6
                            local.get 1
                            i32.load8_u offset=128
                            local.tee 8
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 6
                            local.get 0
                            i64.const 4
                            i64.add
                            local.tee 0
                            i64.gt_u
                            br_if 0 (;@12;)
                            local.get 4
                            local.get 0
                            i64.le_u
                            br_if 0 (;@12;)
                            i32.const 1
                            local.set 8
                            local.get 0
                            i32.load
                            local.tee 2
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 4 (;@8;)
                            local.get 2
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.get 4
                            i64.lt_u
                            br_if 4 (;@8;)
                            global.get $__memory_base
                            i64.const 3832
                            i64.add
                            i64.load
                            i64.const 0
                            i64.eq
                            br_if 3 (;@9;)
                            global.get $__memory_base
                            i64.const 3832
                            i64.add
                            i64.load offset=8
                            local.tee 3
                            i32.wrap_i64
                            local.get 3
                            i32.load offset=4
                            i32.const 4
                            i32.shr_u
                            i32.const -1
                            i32.add
                            local.get 0
                            i32.wrap_i64
                            local.tee 2
                            i32.const 3
                            i32.shr_u
                            i32.and
                            i32.const 4
                            i32.shl
                            i32.add
                            i32.const 8
                            i32.add
                            i64.extend_i32_u
                            local.tee 3
                            i32.load
                            local.tee 9
                            i32.eqz
                            br_if 2 (;@10;)
                            local.get 9
                            local.get 2
                            i32.eq
                            br_if 4 (;@8;)
                            br 1 (;@11;)
                          end
                          local.get 8
                          local.set 9
                          br 5 (;@6;)
                        end
                        loop  ;; label = @11
                          local.get 3
                          i64.load offset=8
                          local.tee 0
                          i64.const 0
                          i64.eq
                          br_if 1 (;@10;)
                          local.get 0
                          i64.const 8
                          i64.add
                          local.set 3
                          local.get 0
                          i32.load offset=8
                          local.get 2
                          i32.eq
                          br_if 3 (;@8;)
                          br 0 (;@11;)
                        end
                      end
                      global.get $__memory_base
                      local.tee 1
                      i64.const 2497
                      i64.add
                      i64.const 111
                      local.get 1
                      i64.const 3568
                      i64.add
                      call $core::panicking::panic::h1108c90903a88b24
                      unreachable
                    end
                    global.get $__memory_base
                    local.tee 1
                    i64.const 148
                    i64.add
                    i64.const 43
                    local.get 1
                    i64.const 3544
                    i64.add
                    call $core::panicking::panic::h1108c90903a88b24
                    unreachable
                  end
                  i32.const 1
                  local.set 9
                  br 1 (;@6;)
                end
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 0
                    i32.load offset=4
                    local.tee 13
                    local.get 2
                    i32.const 0
                    local.get 2
                    i32.const 31
                    i32.gt_u
                    select
                    local.tee 2
                    i32.sub
                    i32.const 255
                    i32.gt_u
                    br_if 0 (;@8;)
                    local.get 0
                    i32.const 5
                    i32.store
                    br 1 (;@7;)
                  end
                  local.get 0
                  local.get 2
                  i32.const 255
                  i32.add
                  local.tee 13
                  i32.store
                  local.get 0
                  call $motoko_rts::gc::generational::mark_stack::push_mark_stack::h75ccea4977ad5e46
                end
                local.get 2
                local.get 13
                i32.ge_u
                br_if 0 (;@6;)
                local.get 0
                i64.const 8
                i64.add
                local.set 11
                local.get 2
                i64.extend_i32_u
                local.set 7
                loop  ;; label = @7
                  block  ;; label = @8
                    local.get 11
                    local.get 7
                    i64.const 2
                    i64.shl
                    i64.add
                    local.tee 0
                    i32.load
                    local.tee 2
                    i32.const 1
                    i32.eq
                    br_if 0 (;@8;)
                    local.get 2
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 2
                    i64.extend_i32_u
                    i64.const 1
                    i64.add
                    local.get 10
                    i64.lt_u
                    br_if 0 (;@8;)
                    local.get 1
                    i64.const 72
                    i64.add
                    local.get 2
                    call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::mark_object::hf019baa4123d2d6e
                    local.get 1
                    i64.load offset=96
                    local.set 4
                    local.get 1
                    i64.load offset=88
                    local.set 6
                    block  ;; label = @9
                      local.get 1
                      i32.load8_u offset=128
                      local.tee 8
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 6
                      local.get 0
                      i64.gt_u
                      br_if 0 (;@9;)
                      local.get 4
                      local.get 0
                      i64.le_u
                      br_if 0 (;@9;)
                      i32.const 1
                      local.set 8
                      block  ;; label = @10
                        local.get 0
                        i32.load
                        local.tee 2
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 0 (;@10;)
                        local.get 2
                        i64.extend_i32_u
                        i64.const 1
                        i64.add
                        local.get 4
                        i64.lt_u
                        br_if 0 (;@10;)
                        block  ;; label = @11
                          global.get $__memory_base
                          i64.const 3832
                          i64.add
                          i64.load
                          i64.const 0
                          i64.eq
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            global.get $__memory_base
                            i64.const 3832
                            i64.add
                            i64.load offset=8
                            local.tee 3
                            i32.wrap_i64
                            local.get 3
                            i32.load offset=4
                            i32.const 4
                            i32.shr_u
                            i32.const -1
                            i32.add
                            local.get 0
                            i32.wrap_i64
                            local.tee 2
                            i32.const 3
                            i32.shr_u
                            i32.and
                            i32.const 4
                            i32.shl
                            i32.add
                            i32.const 8
                            i32.add
                            i64.extend_i32_u
                            local.tee 3
                            i32.load
                            local.tee 9
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 9
                            local.get 2
                            i32.eq
                            br_if 2 (;@10;)
                            loop  ;; label = @13
                              local.get 3
                              i64.load offset=8
                              local.tee 0
                              i64.const 0
                              i64.eq
                              br_if 1 (;@12;)
                              local.get 0
                              i64.const 8
                              i64.add
                              local.set 3
                              local.get 0
                              i32.load offset=8
                              local.get 2
                              i32.eq
                              br_if 3 (;@10;)
                              br 0 (;@13;)
                            end
                          end
                          global.get $__memory_base
                          local.tee 1
                          i64.const 2497
                          i64.add
                          i64.const 111
                          local.get 1
                          i64.const 3568
                          i64.add
                          call $core::panicking::panic::h1108c90903a88b24
                          unreachable
                        end
                        global.get $__memory_base
                        local.tee 1
                        i64.const 148
                        i64.add
                        i64.const 43
                        local.get 1
                        i64.const 3544
                        i64.add
                        call $core::panicking::panic::h1108c90903a88b24
                        unreachable
                      end
                      i32.const 1
                      local.set 9
                      br 1 (;@8;)
                    end
                    local.get 8
                    local.set 9
                  end
                  local.get 13
                  local.get 7
                  i64.const 1
                  i64.add
                  local.tee 7
                  i32.wrap_i64
                  i32.ne
                  br_if 0 (;@7;)
                end
              end
              global.get $__memory_base
              local.tee 3
              i64.const 3824
              i64.add
              i64.load
              local.tee 0
              local.get 3
              i64.const 3808
              i64.add
              i64.load
              i64.ne
              br_if 0 (;@5;)
            end
          end
          local.get 1
          i64.load offset=120
          f64.convert_i64_u
          local.get 1
          i64.load offset=104
          local.tee 3
          local.get 6
          local.get 4
          local.get 8
          i32.const 255
          i32.and
          select
          local.tee 7
          i64.sub
          f64.convert_i64_u
          f64.div
          f64.const 0x1.e666666666666p-1 (;=0.95;)
          f64.lt
          i32.eqz
          br_if 2 (;@1;)
          i64.const 0
          local.set 0
          block  ;; label = @4
            global.get $__memory_base
            i64.const 3880
            i64.add
            i64.load
            local.tee 3
            i64.const -4
            i64.add
            i32.load
            local.tee 2
            i32.const 8
            i32.lt_u
            br_if 0 (;@4;)
            local.get 3
            i64.load align=1
            local.set 0
          end
          local.get 1
          local.get 0
          i64.store offset=144
          local.get 1
          local.get 0
          i64.clz
          i64.store32 offset=160
          local.get 1
          local.get 3
          global.get $__memory_base
          i64.const 3872
          i64.add
          i64.load
          i64.sub
          i32.wrap_i64
          local.tee 9
          i32.const 3
          i32.shl
          i32.store offset=156
          local.get 1
          local.get 2
          local.get 9
          i32.add
          i32.const 3
          i32.shl
          i32.store offset=152
          block  ;; label = @4
            local.get 1
            i64.const 144
            i64.add
            call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
            local.tee 2
            i32.const -1
            i32.eq
            br_if 0 (;@4;)
            loop  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          local.get 2
                                          i32.const 3
                                          i32.shl
                                          local.tee 2
                                          i64.extend_i32_u
                                          local.tee 0
                                          i32.load
                                          local.tee 9
                                          i32.const -1
                                          i32.add
                                          br_table 0 (;@19;) 1 (;@18;) 4 (;@15;) 1 (;@18;) 12 (;@7;) 1 (;@18;) 13 (;@6;) 1 (;@18;) 9 (;@10;) 1 (;@18;) 8 (;@11;) 1 (;@18;) 7 (;@12;) 1 (;@18;) 6 (;@13;) 1 (;@18;) 13 (;@6;) 1 (;@18;) 2 (;@17;) 1 (;@18;) 13 (;@6;) 1 (;@18;) 13 (;@6;) 1 (;@18;) 5 (;@14;) 1 (;@18;) 3 (;@16;) 1 (;@18;) 2 (;@17;) 1 (;@18;) 2 (;@17;) 1 (;@18;)
                                        end
                                        local.get 0
                                        i32.load offset=4
                                        local.tee 9
                                        i32.eqz
                                        br_if 12 (;@6;)
                                        local.get 0
                                        i64.const 12
                                        i64.add
                                        local.set 10
                                        local.get 9
                                        i64.extend_i32_u
                                        local.set 11
                                        i64.const 0
                                        local.set 0
                                        loop  ;; label = @19
                                          block  ;; label = @20
                                            local.get 10
                                            local.get 0
                                            i64.const 2
                                            i64.shl
                                            i64.add
                                            local.tee 3
                                            i32.load
                                            local.tee 9
                                            local.get 2
                                            i32.ge_u
                                            br_if 0 (;@20;)
                                            local.get 9
                                            i32.const 1
                                            i32.eq
                                            br_if 0 (;@20;)
                                            local.get 9
                                            i32.const 1
                                            i32.and
                                            i32.eqz
                                            br_if 0 (;@20;)
                                            local.get 9
                                            i64.extend_i32_u
                                            i64.const 1
                                            i64.add
                                            local.get 7
                                            i64.lt_u
                                            br_if 0 (;@20;)
                                            local.get 1
                                            i64.const 72
                                            i64.add
                                            local.get 3
                                            call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                                          end
                                          local.get 0
                                          i64.const 1
                                          i64.add
                                          local.tee 0
                                          local.get 11
                                          i64.ne
                                          br_if 0 (;@19;)
                                          br 13 (;@6;)
                                        end
                                      end
                                      local.get 9
                                      i32.const 31
                                      i32.gt_u
                                      br_if 10 (;@7;)
                                    end
                                    global.get $__memory_base
                                    i64.const 1902
                                    i64.add
                                    i64.const 42
                                    call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                    unreachable
                                  end
                                  global.get $__memory_base
                                  i64.const 1944
                                  i64.add
                                  i64.const 51
                                  call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                  unreachable
                                end
                                local.get 0
                                i32.load offset=4
                                local.tee 9
                                local.get 2
                                i32.ge_u
                                br_if 8 (;@6;)
                                local.get 9
                                i32.const 1
                                i32.eq
                                br_if 8 (;@6;)
                                local.get 9
                                i32.const 1
                                i32.and
                                i32.eqz
                                br_if 8 (;@6;)
                                local.get 9
                                i64.extend_i32_u
                                i64.const 1
                                i64.add
                                local.get 7
                                i64.lt_u
                                br_if 8 (;@6;)
                                br 5 (;@9;)
                              end
                              block  ;; label = @14
                                local.get 0
                                i32.load offset=8
                                local.tee 9
                                local.get 2
                                i32.ge_u
                                br_if 0 (;@14;)
                                local.get 9
                                i32.const 1
                                i32.eq
                                br_if 0 (;@14;)
                                local.get 9
                                i32.const 1
                                i32.and
                                i32.eqz
                                br_if 0 (;@14;)
                                local.get 9
                                i64.extend_i32_u
                                i64.const 1
                                i64.add
                                local.get 7
                                i64.lt_u
                                br_if 0 (;@14;)
                                local.get 1
                                i64.const 72
                                i64.add
                                local.get 0
                                i64.const 8
                                i64.add
                                call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                              end
                              local.get 0
                              i32.load offset=12
                              local.tee 9
                              local.get 2
                              i32.ge_u
                              br_if 7 (;@6;)
                              local.get 9
                              i32.const 1
                              i32.eq
                              br_if 7 (;@6;)
                              local.get 9
                              i32.const 1
                              i32.and
                              i32.eqz
                              br_if 7 (;@6;)
                              local.get 9
                              i64.extend_i32_u
                              i64.const 1
                              i64.add
                              local.get 7
                              i64.lt_u
                              br_if 7 (;@6;)
                              local.get 0
                              i64.const 12
                              i64.add
                              local.set 0
                              br 5 (;@8;)
                            end
                            local.get 0
                            i32.load offset=8
                            local.tee 9
                            local.get 2
                            i32.ge_u
                            br_if 6 (;@6;)
                            local.get 9
                            i32.const 1
                            i32.eq
                            br_if 6 (;@6;)
                            local.get 9
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 6 (;@6;)
                            local.get 9
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.get 7
                            i64.lt_u
                            br_if 6 (;@6;)
                            local.get 0
                            i64.const 8
                            i64.add
                            local.set 0
                            br 4 (;@8;)
                          end
                          local.get 0
                          i32.load offset=4
                          local.tee 9
                          local.get 2
                          i32.ge_u
                          br_if 5 (;@6;)
                          local.get 9
                          i32.const 1
                          i32.eq
                          br_if 5 (;@6;)
                          local.get 9
                          i32.const 1
                          i32.and
                          i32.eqz
                          br_if 5 (;@6;)
                          local.get 9
                          i64.extend_i32_u
                          i64.const 1
                          i64.add
                          local.get 7
                          i64.ge_u
                          br_if 2 (;@9;)
                          br 5 (;@6;)
                        end
                        local.get 0
                        i32.load offset=8
                        local.tee 9
                        i32.eqz
                        br_if 4 (;@6;)
                        local.get 0
                        i64.const 12
                        i64.add
                        local.set 10
                        local.get 9
                        i64.extend_i32_u
                        local.set 11
                        i64.const 0
                        local.set 0
                        loop  ;; label = @11
                          block  ;; label = @12
                            local.get 10
                            local.get 0
                            i64.const 2
                            i64.shl
                            i64.add
                            local.tee 3
                            i32.load
                            local.tee 9
                            local.get 2
                            i32.ge_u
                            br_if 0 (;@12;)
                            local.get 9
                            i32.const 1
                            i32.eq
                            br_if 0 (;@12;)
                            local.get 9
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 9
                            i64.extend_i32_u
                            i64.const 1
                            i64.add
                            local.get 7
                            i64.lt_u
                            br_if 0 (;@12;)
                            local.get 1
                            i64.const 72
                            i64.add
                            local.get 3
                            call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                          end
                          local.get 0
                          i64.const 1
                          i64.add
                          local.tee 0
                          local.get 11
                          i64.ne
                          br_if 0 (;@11;)
                          br 5 (;@6;)
                        end
                      end
                      local.get 0
                      i32.load offset=4
                      local.tee 9
                      local.get 2
                      i32.ge_u
                      br_if 3 (;@6;)
                      local.get 9
                      i32.const 1
                      i32.eq
                      br_if 3 (;@6;)
                      local.get 9
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 3 (;@6;)
                      local.get 9
                      i64.extend_i32_u
                      i64.const 1
                      i64.add
                      local.get 7
                      i64.lt_u
                      br_if 3 (;@6;)
                    end
                    local.get 0
                    i64.const 4
                    i64.add
                    local.set 0
                  end
                  local.get 1
                  i64.const 72
                  i64.add
                  local.get 0
                  call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                  br 1 (;@6;)
                end
                local.get 9
                i32.const 0
                local.get 9
                i32.const 31
                i32.gt_u
                select
                local.tee 9
                local.get 0
                i32.load offset=4
                local.tee 13
                i32.ge_u
                br_if 0 (;@6;)
                local.get 0
                i64.const 8
                i64.add
                local.set 10
                local.get 9
                i64.extend_i32_u
                local.set 0
                loop  ;; label = @7
                  block  ;; label = @8
                    local.get 10
                    local.get 0
                    i64.const 2
                    i64.shl
                    i64.add
                    local.tee 3
                    i32.load
                    local.tee 9
                    local.get 2
                    i32.ge_u
                    br_if 0 (;@8;)
                    local.get 9
                    i32.const 1
                    i32.eq
                    br_if 0 (;@8;)
                    local.get 9
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 9
                    i64.extend_i32_u
                    i64.const 1
                    i64.add
                    local.get 7
                    i64.lt_u
                    br_if 0 (;@8;)
                    local.get 1
                    i64.const 72
                    i64.add
                    local.get 3
                    call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                  end
                  local.get 13
                  local.get 0
                  i64.const 1
                  i64.add
                  local.tee 0
                  i32.wrap_i64
                  i32.ne
                  br_if 0 (;@7;)
                end
              end
              local.get 1
              i64.const 144
              i64.add
              call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
              local.tee 2
              i32.const -1
              i32.ne
              br_if 0 (;@5;)
            end
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i64.load32_u offset=80
                  local.tee 0
                  i64.const 5
                  i64.add
                  i32.load
                  local.tee 2
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 0
                  i64.const 1
                  i64.add
                  i64.const 8
                  i64.add
                  local.set 10
                  local.get 2
                  i64.extend_i32_u
                  local.set 11
                  i64.const 0
                  local.set 0
                  loop  ;; label = @8
                    local.get 1
                    local.get 0
                    i64.const 3
                    i64.shl
                    i64.const 4294967288
                    i64.and
                    local.get 10
                    i64.add
                    i64.load32_u
                    local.tee 3
                    i32.load offset=1
                    local.tee 2
                    i32.store offset=140
                    local.get 2
                    i32.const 9
                    i32.ne
                    br_if 2 (;@6;)
                    local.get 6
                    local.get 3
                    i64.const 1
                    i64.add
                    local.tee 3
                    i64.le_u
                    br_if 3 (;@5;)
                    block  ;; label = @9
                      local.get 3
                      i32.load offset=4
                      local.tee 2
                      i32.const 1
                      i32.eq
                      br_if 0 (;@9;)
                      local.get 2
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 2
                      i64.extend_i32_u
                      i64.const 1
                      i64.add
                      local.get 7
                      i64.lt_u
                      br_if 0 (;@9;)
                      local.get 1
                      i64.const 72
                      i64.add
                      local.get 3
                      i64.const 4
                      i64.add
                      call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                    end
                    local.get 0
                    i64.const 1
                    i64.add
                    local.tee 0
                    local.get 11
                    i64.ne
                    br_if 0 (;@8;)
                  end
                end
                local.get 1
                i64.load offset=72
                local.tee 0
                i32.load
                local.tee 2
                i32.const 1
                i32.eq
                br_if 4 (;@2;)
                local.get 2
                i32.const 1
                i32.and
                i32.eqz
                br_if 4 (;@2;)
                local.get 2
                i64.extend_i32_u
                i64.const 1
                i64.add
                local.get 7
                i64.ge_u
                br_if 2 (;@4;)
                br 4 (;@2;)
              end
              local.get 1
              i64.const 0
              i64.store offset=160
              local.get 1
              i64.const 140
              i64.add
              global.get $__memory_base
              local.tee 0
              i64.const 2376
              i64.add
              local.get 1
              i64.const 144
              i64.add
              local.get 0
              i64.const 3592
              i64.add
              call $core::panicking::assert_failed::ha6cca40bd32447e3
              unreachable
            end
            global.get $__memory_base
            local.tee 1
            i64.const 2380
            i64.add
            i64.const 59
            local.get 1
            i64.const 3616
            i64.add
            call $core::panicking::panic::h1108c90903a88b24
            unreachable
          end
          local.get 1
          i64.const 72
          i64.add
          local.get 0
          call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
          br 1 (;@2;)
        end
        global.get $__memory_base
        local.tee 1
        i64.const 148
        i64.add
        i64.const 43
        local.get 1
        i64.const 3472
        i64.add
        call $core::panicking::panic::h1108c90903a88b24
        unreachable
      end
      block  ;; label = @2
        block  ;; label = @3
          local.get 8
          i32.const 255
          i32.and
          br_if 0 (;@3;)
          global.get $__memory_base
          i64.const 3832
          i64.add
          i64.load
          i64.const 0
          i64.eq
          br_if 1 (;@2;)
          global.get $__memory_base
          i64.const 3832
          i64.add
          i64.load offset=8
          local.tee 0
          i64.const 8
          i64.add
          i64.const 4294967295
          i64.and
          local.tee 3
          i32.load
          local.set 2
          local.get 1
          i32.const 0
          i32.store offset=160
          local.get 1
          local.get 0
          i64.store offset=144
          local.get 1
          local.get 3
          i64.const 0
          local.get 2
          select
          i64.store offset=152
          local.get 1
          i64.const 144
          i64.add
          call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
          local.get 1
          i64.load offset=152
          local.tee 3
          i64.eqz
          br_if 0 (;@3;)
          loop  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 6
                  local.get 3
                  i64.load32_u
                  local.tee 0
                  i64.gt_u
                  br_if 0 (;@7;)
                  local.get 4
                  local.get 0
                  i64.le_u
                  br_if 0 (;@7;)
                  local.get 0
                  i32.load
                  local.tee 2
                  i32.const 1
                  i32.and
                  i32.eqz
                  br_if 2 (;@5;)
                  local.get 2
                  i64.extend_i32_u
                  i64.const 1
                  i64.add
                  local.get 4
                  i64.ge_u
                  br_if 1 (;@6;)
                  br 2 (;@5;)
                end
                global.get $__memory_base
                local.tee 1
                i64.const 2608
                i64.add
                i64.const 119
                local.get 1
                i64.const 3664
                i64.add
                call $core::panicking::panic::h1108c90903a88b24
                unreachable
              end
              local.get 1
              i64.const 72
              i64.add
              local.get 0
              call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
            end
            local.get 1
            i64.const 0
            local.get 3
            i64.load offset=8
            local.tee 0
            i64.const 8
            i64.add
            local.get 0
            i64.eqz
            select
            i64.store offset=152
            local.get 1
            i64.const 144
            i64.add
            call $motoko_rts::gc::generational::remembered_set::RememberedSetIterator::skip_free::hf6667060fda4734f
            local.get 1
            i64.load offset=152
            local.tee 3
            i64.eqz
            i32.eqz
            br_if 0 (;@4;)
          end
        end
        i64.const 0
        local.set 0
        global.get $__memory_base
        local.tee 3
        i64.const 3832
        i64.add
        i64.const 0
        i64.store
        block  ;; label = @3
          local.get 3
          i64.const 3880
          i64.add
          i64.load
          local.tee 3
          i64.const -4
          i64.add
          i32.load
          local.tee 2
          i32.const 8
          i32.lt_u
          br_if 0 (;@3;)
          local.get 3
          i64.load align=1
          local.set 0
        end
        local.get 1
        local.get 0
        i64.store offset=144
        local.get 1
        local.get 0
        i64.clz
        i64.store32 offset=160
        local.get 1
        local.get 3
        global.get $__memory_base
        i64.const 3872
        i64.add
        i64.load
        i64.sub
        i32.wrap_i64
        local.tee 8
        i32.const 3
        i32.shl
        i32.store offset=156
        local.get 1
        local.get 2
        local.get 8
        i32.add
        i32.const 3
        i32.shl
        i32.store offset=152
        block  ;; label = @3
          local.get 1
          i64.const 144
          i64.add
          call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
          local.tee 2
          i32.const -1
          i32.ne
          br_if 0 (;@3;)
          local.get 7
          local.set 3
          br 2 (;@1;)
        end
        local.get 7
        local.set 3
        loop  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 7
                local.get 2
                i32.const 3
                i32.shl
                i64.extend_i32_u
                local.tee 4
                i64.gt_u
                br_if 0 (;@6;)
                block  ;; label = @7
                  local.get 4
                  i32.load
                  local.tee 2
                  i32.const 1
                  i32.and
                  br_if 0 (;@7;)
                  local.get 3
                  i32.wrap_i64
                  i32.const -1
                  i32.add
                  local.set 8
                  loop  ;; label = @8
                    local.get 2
                    i64.extend_i32_u
                    local.tee 0
                    i32.load
                    local.set 2
                    local.get 0
                    local.get 8
                    i32.store
                    local.get 2
                    i32.const 1
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                  end
                end
                local.get 2
                i32.const 28
                i32.ge_u
                br_if 1 (;@5;)
                local.get 4
                local.get 2
                i32.store
                local.get 4
                call $motoko_rts::types::block_size::h0bf0d784741e3e30
                local.set 2
                block  ;; label = @7
                  local.get 3
                  local.get 4
                  i64.ne
                  br_if 0 (;@7;)
                  local.get 2
                  i32.const 3
                  i32.shl
                  i64.extend_i32_u
                  local.set 10
                  br 3 (;@4;)
                end
                local.get 3
                local.get 4
                local.get 2
                i32.const 3
                i32.shl
                i64.extend_i32_u
                local.tee 10
                memory.copy
                br 2 (;@4;)
              end
              global.get $__memory_base
              local.tee 1
              i64.const 2777
              i64.add
              i64.const 49
              local.get 1
              i64.const 3712
              i64.add
              call $core::panicking::panic::h1108c90903a88b24
              unreachable
            end
            global.get $__memory_base
            local.tee 1
            i64.const 2826
            i64.add
            i64.const 60
            local.get 1
            i64.const 3736
            i64.add
            call $core::panicking::panic::h1108c90903a88b24
            unreachable
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      local.get 3
                                      i32.load
                                      local.tee 2
                                      i32.const -1
                                      i32.add
                                      br_table 0 (;@17;) 1 (;@16;) 4 (;@13;) 1 (;@16;) 12 (;@5;) 1 (;@16;) 13 (;@4;) 1 (;@16;) 9 (;@8;) 1 (;@16;) 8 (;@9;) 1 (;@16;) 7 (;@10;) 1 (;@16;) 6 (;@11;) 1 (;@16;) 13 (;@4;) 1 (;@16;) 2 (;@15;) 1 (;@16;) 13 (;@4;) 1 (;@16;) 13 (;@4;) 1 (;@16;) 5 (;@12;) 1 (;@16;) 3 (;@14;) 1 (;@16;) 2 (;@15;) 1 (;@16;) 2 (;@15;) 1 (;@16;)
                                    end
                                    local.get 3
                                    i32.load offset=4
                                    local.tee 2
                                    i32.eqz
                                    br_if 12 (;@4;)
                                    local.get 3
                                    i64.const 12
                                    i64.add
                                    local.set 11
                                    local.get 2
                                    i64.extend_i32_u
                                    local.set 12
                                    i64.const 0
                                    local.set 0
                                    loop  ;; label = @17
                                      block  ;; label = @18
                                        local.get 3
                                        local.get 11
                                        local.get 0
                                        i64.const 2
                                        i64.shl
                                        i64.add
                                        local.tee 6
                                        i32.load
                                        local.tee 2
                                        i64.extend_i32_u
                                        local.tee 4
                                        i64.gt_u
                                        br_if 0 (;@18;)
                                        local.get 2
                                        i32.const 1
                                        i32.eq
                                        br_if 0 (;@18;)
                                        local.get 2
                                        i32.const 1
                                        i32.and
                                        i32.eqz
                                        br_if 0 (;@18;)
                                        local.get 4
                                        i64.const 1
                                        i64.add
                                        local.get 7
                                        i64.lt_u
                                        br_if 0 (;@18;)
                                        local.get 1
                                        i64.const 72
                                        i64.add
                                        local.get 6
                                        call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                                      end
                                      local.get 0
                                      i64.const 1
                                      i64.add
                                      local.tee 0
                                      local.get 12
                                      i64.ne
                                      br_if 0 (;@17;)
                                      br 13 (;@4;)
                                    end
                                  end
                                  local.get 2
                                  i32.const 31
                                  i32.gt_u
                                  br_if 10 (;@5;)
                                end
                                global.get $__memory_base
                                i64.const 1902
                                i64.add
                                i64.const 42
                                call $motoko_rts::rts_trap_with::h000639acaf03dda1
                                unreachable
                              end
                              global.get $__memory_base
                              i64.const 1944
                              i64.add
                              i64.const 51
                              call $motoko_rts::rts_trap_with::h000639acaf03dda1
                              unreachable
                            end
                            local.get 3
                            local.get 3
                            i32.load offset=4
                            local.tee 2
                            i64.extend_i32_u
                            local.tee 0
                            i64.gt_u
                            br_if 8 (;@4;)
                            local.get 2
                            i32.const 1
                            i32.eq
                            br_if 8 (;@4;)
                            local.get 2
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 8 (;@4;)
                            local.get 0
                            i64.const 1
                            i64.add
                            local.get 7
                            i64.lt_u
                            br_if 8 (;@4;)
                            br 5 (;@7;)
                          end
                          block  ;; label = @12
                            local.get 3
                            local.get 3
                            i32.load offset=8
                            local.tee 2
                            i64.extend_i32_u
                            local.tee 0
                            i64.gt_u
                            br_if 0 (;@12;)
                            local.get 2
                            i32.const 1
                            i32.eq
                            br_if 0 (;@12;)
                            local.get 2
                            i32.const 1
                            i32.and
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 0
                            i64.const 1
                            i64.add
                            local.get 7
                            i64.lt_u
                            br_if 0 (;@12;)
                            local.get 1
                            i64.const 72
                            i64.add
                            local.get 3
                            i64.const 8
                            i64.add
                            call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                          end
                          local.get 3
                          local.get 3
                          i32.load offset=12
                          local.tee 2
                          i64.extend_i32_u
                          local.tee 0
                          i64.gt_u
                          br_if 7 (;@4;)
                          local.get 2
                          i32.const 1
                          i32.eq
                          br_if 7 (;@4;)
                          local.get 2
                          i32.const 1
                          i32.and
                          i32.eqz
                          br_if 7 (;@4;)
                          local.get 0
                          i64.const 1
                          i64.add
                          local.get 7
                          i64.lt_u
                          br_if 7 (;@4;)
                          local.get 3
                          i64.const 12
                          i64.add
                          local.set 0
                          br 5 (;@6;)
                        end
                        local.get 3
                        local.get 3
                        i32.load offset=8
                        local.tee 2
                        i64.extend_i32_u
                        local.tee 0
                        i64.gt_u
                        br_if 6 (;@4;)
                        local.get 2
                        i32.const 1
                        i32.eq
                        br_if 6 (;@4;)
                        local.get 2
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 6 (;@4;)
                        local.get 0
                        i64.const 1
                        i64.add
                        local.get 7
                        i64.lt_u
                        br_if 6 (;@4;)
                        local.get 3
                        i64.const 8
                        i64.add
                        local.set 0
                        br 4 (;@6;)
                      end
                      local.get 3
                      local.get 3
                      i32.load offset=4
                      local.tee 2
                      i64.extend_i32_u
                      local.tee 0
                      i64.gt_u
                      br_if 5 (;@4;)
                      local.get 2
                      i32.const 1
                      i32.eq
                      br_if 5 (;@4;)
                      local.get 2
                      i32.const 1
                      i32.and
                      i32.eqz
                      br_if 5 (;@4;)
                      local.get 0
                      i64.const 1
                      i64.add
                      local.get 7
                      i64.ge_u
                      br_if 2 (;@7;)
                      br 5 (;@4;)
                    end
                    local.get 3
                    i32.load offset=8
                    local.tee 2
                    i32.eqz
                    br_if 4 (;@4;)
                    local.get 3
                    i64.const 12
                    i64.add
                    local.set 11
                    local.get 2
                    i64.extend_i32_u
                    local.set 12
                    i64.const 0
                    local.set 0
                    loop  ;; label = @9
                      block  ;; label = @10
                        local.get 3
                        local.get 11
                        local.get 0
                        i64.const 2
                        i64.shl
                        i64.add
                        local.tee 6
                        i32.load
                        local.tee 2
                        i64.extend_i32_u
                        local.tee 4
                        i64.gt_u
                        br_if 0 (;@10;)
                        local.get 2
                        i32.const 1
                        i32.eq
                        br_if 0 (;@10;)
                        local.get 2
                        i32.const 1
                        i32.and
                        i32.eqz
                        br_if 0 (;@10;)
                        local.get 4
                        i64.const 1
                        i64.add
                        local.get 7
                        i64.lt_u
                        br_if 0 (;@10;)
                        local.get 1
                        i64.const 72
                        i64.add
                        local.get 6
                        call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
                      end
                      local.get 0
                      i64.const 1
                      i64.add
                      local.tee 0
                      local.get 12
                      i64.ne
                      br_if 0 (;@9;)
                      br 5 (;@4;)
                    end
                  end
                  local.get 3
                  local.get 3
                  i32.load offset=4
                  local.tee 2
                  i64.extend_i32_u
                  local.tee 0
                  i64.gt_u
                  br_if 3 (;@4;)
                  local.get 2
                  i32.const 1
                  i32.eq
                  br_if 3 (;@4;)
                  local.get 2
                  i32.const 1
                  i32.and
                  i32.eqz
                  br_if 3 (;@4;)
                  local.get 0
                  i64.const 1
                  i64.add
                  local.get 7
                  i64.lt_u
                  br_if 3 (;@4;)
                end
                local.get 3
                i64.const 4
                i64.add
                local.set 0
              end
              local.get 1
              i64.const 72
              i64.add
              local.get 0
              call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
              br 1 (;@4;)
            end
            local.get 2
            i32.const 0
            local.get 2
            i32.const 31
            i32.gt_u
            select
            local.tee 2
            local.get 3
            i32.load offset=4
            local.tee 8
            i32.ge_u
            br_if 0 (;@4;)
            local.get 3
            i64.const 8
            i64.add
            local.set 11
            local.get 2
            i64.extend_i32_u
            local.set 0
            loop  ;; label = @5
              block  ;; label = @6
                local.get 3
                local.get 11
                local.get 0
                i64.const 2
                i64.shl
                i64.add
                local.tee 6
                i32.load
                local.tee 2
                i64.extend_i32_u
                local.tee 4
                i64.gt_u
                br_if 0 (;@6;)
                local.get 2
                i32.const 1
                i32.eq
                br_if 0 (;@6;)
                local.get 2
                i32.const 1
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                local.get 4
                i64.const 1
                i64.add
                local.get 7
                i64.lt_u
                br_if 0 (;@6;)
                local.get 1
                i64.const 72
                i64.add
                local.get 6
                call $motoko_rts::gc::generational::GenerationalGC$LT$M$GT$::thread::h02d84222c2509c7b
              end
              local.get 8
              local.get 0
              i64.const 1
              i64.add
              local.tee 0
              i32.wrap_i64
              i32.ne
              br_if 0 (;@5;)
            end
          end
          local.get 10
          local.get 3
          i64.add
          local.set 3
          local.get 1
          i64.const 144
          i64.add
          call $motoko_rts::gc::mark_compact::bitmap::BitmapIter::next::h6f256a8f4b32a705
          local.tee 2
          i32.const -1
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 1
        i64.load offset=88
        local.set 6
        br 1 (;@1;)
      end
      global.get $__memory_base
      local.tee 1
      i64.const 148
      i64.add
      i64.const 43
      local.get 1
      i64.const 3640
      i64.add
      call $core::panicking::panic::h1108c90903a88b24
      unreachable
    end
    global.get $__memory_base
    local.tee 0
    i64.const 3808
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3800
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3824
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3816
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3880
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3872
    i64.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 3928
    i64.add
    local.get 3
    i32.wrap_i64
    local.tee 2
    i32.store
    local.get 0
    i64.const 3932
    i64.add
    local.get 2
    i32.store
    local.get 0
    i64.const 3920
    i64.add
    local.tee 4
    local.get 1
    i64.load offset=16
    local.get 3
    i64.sub
    local.get 4
    i64.load
    i64.add
    i64.store
    local.get 0
    i64.const 3796
    i64.add
    local.tee 0
    local.get 0
    i32.load
    local.tee 8
    local.get 2
    local.get 6
    i32.wrap_i64
    i32.sub
    local.tee 2
    local.get 8
    local.get 2
    i32.gt_u
    select
    i32.store
    block  ;; label = @1
      local.get 5
      i32.eqz
      br_if 0 (;@1;)
      global.get $__memory_base
      i64.const 3368
      i64.add
      local.get 3
      local.get 6
      i64.sub
      f64.convert_i64_u
      local.tee 14
      local.get 14
      f64.add
      i64.trunc_sat_f64_u
      i64.store
      local.get 3
      i64.const 3758096383
      i64.gt_u
      br_if 0 (;@1;)
      global.get $__memory_base
      i64.const 3864
      i64.add
      i32.const 0
      i32.store8
    end
    call $motoko_rts::gc::generational::write_barrier::init_generational_write_barrier::h30dc7201e6243862
    local.get 1
    i64.const 192
    i64.add
    global.set $__stack_pointer)
  (func $generational_gc (type 10)
    (local i64)
    global.get $__stack_pointer
    i64.const 16
    i64.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    i64.const 15
    i64.add
    call $motoko_rts::gc::generational::generational_gc::h7a9385a87cee170d
    local.get 0
    i64.const 16
    i64.add
    global.set $__stack_pointer)
  (func $schedule_generational_gc (type 10)
    (local i64)
    global.get $__stack_pointer
    i64.const 32
    i64.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    call $motoko_rts::gc::generational::get_limits::ha6736df7ee81ff11
    block  ;; label = @1
      local.get 0
      call $motoko_rts::gc::generational::decide_strategy::h7620e853babf9e48
      i32.const 255
      i32.and
      i32.const 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      i64.const 31
      i64.add
      call $motoko_rts::gc::generational::generational_gc::h7a9385a87cee170d
    end
    local.get 0
    i64.const 32
    i64.add
    global.set $__stack_pointer)
  (func $initialize_generational_gc (type 10)
    (local i64 i32)
    global.get $__memory_base
    local.tee 0
    i64.const 3932
    i64.add
    call $get_heap_base
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    local.tee 1
    i32.store
    local.get 0
    i64.const 3928
    i64.add
    local.get 1
    i32.store
    call $motoko_rts::gc::generational::write_barrier::init_generational_write_barrier::h30dc7201e6243862)
  (func $post_write_barrier (type 16) (param i32)
    local.get 0
    call $motoko_rts::gc::generational::write_barrier::post_write_barrier::h4d04fbc9f10cc334)
  (func $motoko_rts::gc::copying::copying_gc::h9c53b67da0316b21 (type 10)
    (local i32 i32 i32 i64 i32 i64 i64 i64 i64)
    call $get_heap_base
    local.set 0
    call $get_static_roots
    local.set 1
    global.get $__memory_base
    i64.const 3928
    i64.add
    i32.load
    local.tee 2
    i64.extend_i32_u
    local.set 3
    local.get 0
    i32.const 31
    i32.add
    i32.const -32
    i32.and
    local.tee 4
    i64.extend_i32_u
    local.set 5
    block  ;; label = @1
      local.get 1
      i64.extend_i32_u
      local.tee 6
      i64.const 5
      i64.add
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 6
      i64.const 1
      i64.add
      i64.const 8
      i64.add
      local.set 7
      local.get 0
      i64.extend_i32_u
      local.set 8
      i64.const 0
      local.set 6
      loop  ;; label = @2
        local.get 5
        local.get 3
        local.get 6
        i64.const 3
        i64.shl
        i64.const 4294967288
        i64.and
        local.get 7
        i64.add
        i32.load
        call $motoko_rts::gc::copying::scav::hcb2e323f2d684a01
        local.get 6
        i64.const 1
        i64.add
        local.tee 6
        local.get 8
        i64.ne
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      global.get $__memory_base
      i64.const 3784
      i64.add
      i32.load
      local.tee 0
      i32.const 1
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 5
      local.get 3
      global.get $__memory_base
      i64.const 3784
      i64.add
      call $motoko_rts::gc::copying::evac::ha29f7cf5a56d2ef4
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        global.get $__memory_base
        i64.const 3928
        i64.add
        i32.load
        local.tee 0
        i32.ge_u
        br_if 0 (;@2;)
        local.get 3
        local.set 6
        loop  ;; label = @3
          local.get 6
          call $motoko_rts::types::block_size::h0bf0d784741e3e30
          local.set 0
          local.get 5
          local.get 3
          local.get 6
          i32.wrap_i64
          i32.const -1
          i32.add
          call $motoko_rts::gc::copying::scav::hcb2e323f2d684a01
          local.get 6
          local.get 0
          i32.const 3
          i32.shl
          i64.extend_i32_u
          i64.add
          local.tee 6
          global.get $__memory_base
          i64.const 3928
          i64.add
          i64.load32_u
          local.tee 7
          i64.lt_u
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      local.get 0
      i64.extend_i32_u
      local.set 7
    end
    global.get $__memory_base
    local.tee 6
    i64.const 3796
    i64.add
    local.tee 8
    local.get 8
    i32.load
    local.tee 1
    local.get 7
    local.get 3
    i64.sub
    local.tee 7
    i32.wrap_i64
    local.tee 0
    local.get 1
    local.get 0
    i32.gt_u
    select
    i32.store
    local.get 6
    i64.const 3920
    i64.add
    local.tee 8
    local.get 8
    i64.load
    local.get 2
    local.get 4
    local.get 0
    i32.add
    local.tee 0
    i32.sub
    i64.extend_i32_u
    i64.add
    i64.store
    local.get 5
    local.get 3
    local.get 7
    i64.const 4294967295
    i64.and
    memory.copy
    local.get 6
    i64.const 3932
    i64.add
    local.get 0
    i32.store
    local.get 6
    i64.const 3928
    i64.add
    local.get 0
    i32.store)
  (func $copying_gc (type 10)
    call $motoko_rts::gc::copying::copying_gc::h9c53b67da0316b21)
  (func $schedule_copying_gc (type 10)
    (local i64 i32 i64 i64)
    block  ;; label = @1
      global.get $__memory_base
      local.tee 0
      i64.const 3932
      i64.add
      i32.load
      local.tee 1
      f64.convert_i32_u
      f64.const 0x1.8p+0 (;=1.5;)
      f64.mul
      i64.trunc_sat_f64_u
      local.tee 2
      local.get 1
      i32.const 1
      i32.shr_u
      i32.const -2147483648
      i32.or
      i64.extend_i32_u
      local.tee 3
      local.get 2
      local.get 3
      i64.lt_u
      select
      local.get 0
      i64.const 3928
      i64.add
      i64.load32_u
      i64.gt_u
      br_if 0 (;@1;)
      call $motoko_rts::gc::copying::copying_gc::h9c53b67da0316b21
    end)
  (func $memcpy (type 22) (param i64 i64 i64) (result i64)
    (local i64 i64 i64 i32 i64 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i64.const 3
        i64.and
        i64.eqz
        br_if 0 (;@2;)
        local.get 2
        i64.eqz
        br_if 0 (;@2;)
        local.get 1
        i64.const 1
        i64.add
        local.set 3
        local.get 0
        local.set 4
        loop  ;; label = @3
          local.get 4
          local.get 1
          i32.load8_u
          i32.store8
          local.get 2
          i64.const -1
          i64.add
          local.set 5
          local.get 4
          i64.const 1
          i64.add
          local.set 4
          local.get 1
          i64.const 1
          i64.add
          local.set 1
          local.get 3
          i64.const 3
          i64.and
          i64.eqz
          br_if 2 (;@1;)
          local.get 3
          i64.const 1
          i64.add
          local.set 3
          local.get 2
          i64.const 1
          i64.ne
          local.set 6
          local.get 5
          local.set 2
          local.get 6
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      local.get 2
      local.set 5
      local.get 0
      local.set 4
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i64.const 3
          i64.and
          local.tee 2
          i64.const 0
          i64.ne
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 5
            i64.const 16
            i64.lt_u
            br_if 0 (;@4;)
            loop  ;; label = @5
              local.get 4
              local.get 1
              i64.load align=4
              i64.store align=4
              local.get 4
              i64.const 8
              i64.add
              local.get 1
              i64.const 8
              i64.add
              i64.load align=4
              i64.store align=4
              local.get 4
              i64.const 16
              i64.add
              local.set 4
              local.get 1
              i64.const 16
              i64.add
              local.set 1
              local.get 5
              i64.const -16
              i64.add
              local.tee 5
              i64.const 15
              i64.gt_u
              br_if 0 (;@5;)
            end
          end
          block  ;; label = @4
            local.get 5
            i64.const 8
            i64.and
            i64.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 1
            i64.load align=4
            i64.store align=4
            local.get 1
            i64.const 8
            i64.add
            local.set 1
            local.get 4
            i64.const 8
            i64.add
            local.set 4
          end
          block  ;; label = @4
            local.get 5
            i64.const 4
            i64.and
            i64.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 1
            i32.load
            i32.store
            local.get 1
            i64.const 4
            i64.add
            local.set 1
            local.get 4
            i64.const 4
            i64.add
            local.set 4
          end
          block  ;; label = @4
            local.get 5
            i64.const 2
            i64.and
            i64.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 1
            i32.load16_u align=1
            i32.store16 align=1
            local.get 4
            i64.const 2
            i64.add
            local.set 4
            local.get 1
            i64.const 2
            i64.add
            local.set 1
          end
          local.get 5
          i64.const 1
          i64.and
          i64.eqz
          i32.eqz
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 5
          i64.const 32
          i64.lt_u
          br_if 0 (;@3;)
          local.get 2
          i64.const -1
          i64.add
          local.tee 2
          i64.const 2
          i64.gt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 2
                i32.wrap_i64
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;) 0 (;@6;)
              end
              local.get 4
              local.get 1
              i32.load
              local.tee 6
              i32.store8
              local.get 4
              local.get 6
              i32.const 16
              i32.shr_u
              i32.store8 offset=2
              local.get 4
              local.get 6
              i32.const 8
              i32.shr_u
              i32.store8 offset=1
              local.get 5
              i64.const -3
              i64.add
              local.set 5
              local.get 4
              i64.const 3
              i64.add
              local.set 7
              i64.const 0
              local.set 2
              loop  ;; label = @6
                local.get 7
                local.get 2
                i64.add
                local.tee 4
                local.get 1
                local.get 2
                i64.add
                local.tee 3
                i64.const 4
                i64.add
                i32.load
                local.tee 8
                i32.const 8
                i32.shl
                local.get 6
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i64.const 4
                i64.add
                local.get 3
                i64.const 8
                i64.add
                i32.load
                local.tee 6
                i32.const 8
                i32.shl
                local.get 8
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i64.const 8
                i64.add
                local.get 3
                i64.const 12
                i64.add
                i32.load
                local.tee 8
                i32.const 8
                i32.shl
                local.get 6
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i64.const 12
                i64.add
                local.get 3
                i64.const 16
                i64.add
                i32.load
                local.tee 6
                i32.const 8
                i32.shl
                local.get 8
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 2
                i64.const 16
                i64.add
                local.set 2
                local.get 5
                i64.const -16
                i64.add
                local.tee 5
                i64.const 16
                i64.gt_u
                br_if 0 (;@6;)
              end
              local.get 7
              local.get 2
              i64.add
              local.set 4
              local.get 1
              local.get 2
              i64.add
              i64.const 3
              i64.add
              local.set 1
              br 2 (;@3;)
            end
            local.get 4
            local.get 1
            i32.load
            local.tee 6
            i32.store16 align=1
            local.get 5
            i64.const -2
            i64.add
            local.set 5
            local.get 4
            i64.const 2
            i64.add
            local.set 7
            i64.const 0
            local.set 2
            loop  ;; label = @5
              local.get 7
              local.get 2
              i64.add
              local.tee 4
              local.get 1
              local.get 2
              i64.add
              local.tee 3
              i64.const 4
              i64.add
              i32.load
              local.tee 8
              i32.const 16
              i32.shl
              local.get 6
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i64.const 4
              i64.add
              local.get 3
              i64.const 8
              i64.add
              i32.load
              local.tee 6
              i32.const 16
              i32.shl
              local.get 8
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i64.const 8
              i64.add
              local.get 3
              i64.const 12
              i64.add
              i32.load
              local.tee 8
              i32.const 16
              i32.shl
              local.get 6
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i64.const 12
              i64.add
              local.get 3
              i64.const 16
              i64.add
              i32.load
              local.tee 6
              i32.const 16
              i32.shl
              local.get 8
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 2
              i64.const 16
              i64.add
              local.set 2
              local.get 5
              i64.const -16
              i64.add
              local.tee 5
              i64.const 17
              i64.gt_u
              br_if 0 (;@5;)
            end
            local.get 7
            local.get 2
            i64.add
            local.set 4
            local.get 1
            local.get 2
            i64.add
            i64.const 2
            i64.add
            local.set 1
            br 1 (;@3;)
          end
          local.get 4
          local.get 1
          i32.load
          local.tee 6
          i32.store8
          local.get 5
          i64.const -1
          i64.add
          local.set 5
          local.get 4
          i64.const 1
          i64.add
          local.set 7
          i64.const 0
          local.set 2
          loop  ;; label = @4
            local.get 7
            local.get 2
            i64.add
            local.tee 4
            local.get 1
            local.get 2
            i64.add
            local.tee 3
            i64.const 4
            i64.add
            i32.load
            local.tee 8
            i32.const 24
            i32.shl
            local.get 6
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i64.const 4
            i64.add
            local.get 3
            i64.const 8
            i64.add
            i32.load
            local.tee 6
            i32.const 24
            i32.shl
            local.get 8
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i64.const 8
            i64.add
            local.get 3
            i64.const 12
            i64.add
            i32.load
            local.tee 8
            i32.const 24
            i32.shl
            local.get 6
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i64.const 12
            i64.add
            local.get 3
            i64.const 16
            i64.add
            i32.load
            local.tee 6
            i32.const 24
            i32.shl
            local.get 8
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 2
            i64.const 16
            i64.add
            local.set 2
            local.get 5
            i64.const -16
            i64.add
            local.tee 5
            i64.const 18
            i64.gt_u
            br_if 0 (;@4;)
          end
          local.get 7
          local.get 2
          i64.add
          local.set 4
          local.get 1
          local.get 2
          i64.add
          i64.const 1
          i64.add
          local.set 1
        end
        block  ;; label = @3
          local.get 5
          i64.const 16
          i64.and
          i64.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 1
          i32.load8_u
          i32.store8
          local.get 4
          local.get 1
          i32.load offset=1 align=1
          i32.store offset=1 align=1
          local.get 4
          local.get 1
          i64.load offset=5 align=1
          i64.store offset=5 align=1
          local.get 4
          local.get 1
          i32.load16_u offset=13 align=1
          i32.store16 offset=13 align=1
          local.get 4
          local.get 1
          i32.load8_u offset=15
          i32.store8 offset=15
          local.get 4
          i64.const 16
          i64.add
          local.set 4
          local.get 1
          i64.const 16
          i64.add
          local.set 1
        end
        block  ;; label = @3
          local.get 5
          i64.const 8
          i64.and
          i64.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 1
          i64.load align=1
          i64.store align=1
          local.get 4
          i64.const 8
          i64.add
          local.set 4
          local.get 1
          i64.const 8
          i64.add
          local.set 1
        end
        block  ;; label = @3
          local.get 5
          i64.const 4
          i64.and
          i64.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 1
          i32.load align=1
          i32.store align=1
          local.get 4
          i64.const 4
          i64.add
          local.set 4
          local.get 1
          i64.const 4
          i64.add
          local.set 1
        end
        block  ;; label = @3
          local.get 5
          i64.const 2
          i64.and
          i64.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 1
          i32.load16_u align=1
          i32.store16 align=1
          local.get 4
          i64.const 2
          i64.add
          local.set 4
          local.get 1
          i64.const 2
          i64.add
          local.set 1
        end
        local.get 5
        i64.const 1
        i64.and
        i64.eqz
        br_if 1 (;@1;)
      end
      local.get 4
      local.get 1
      i32.load8_u
      i32.store8
    end
    local.get 0)
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "blob_iter_done" (func $blob_iter_done))
  (export "blob_iter_next" (func $blob_iter_next))
  (export "skip_leb128" (func $skip_leb128))
  (export "char_to_upper" (func $char_to_upper))
  (export "char_to_lower" (func $char_to_lower))
  (export "char_is_whitespace" (func $char_is_whitespace))
  (export "char_is_uppercase" (func $char_is_uppercase))
  (export "char_is_lowercase" (func $char_is_lowercase))
  (export "char_is_alphabetic" (func $char_is_alphabetic))
  (export "peek_future_continuation" (func $peek_future_continuation))
  (export "recall_continuation" (func $recall_continuation))
  (export "continuation_count" (func $continuation_count))
  (export "continuation_table_size" (func $continuation_table_size))
  (export "leb128_encode" (func $leb128_encode))
  (export "sleb128_encode" (func $sleb128_encode))
  (export "leb128_decode" (func $leb128_decode))
  (export "sleb128_decode" (func $sleb128_decode))
  (export "get_max_live_size" (func $get_max_live_size))
  (export "compute_crc32" (func $compute_crc32))
  (export "text_to_buf" (func $text_to_buf))
  (export "text_size" (func $text_size))
  (export "text_compare" (func $text_compare))
  (export "text_len" (func $text_len))
  (export "text_iter_done" (func $text_iter_done))
  (export "blob_iter" (func $blob_iter))
  (export "remember_continuation" (func $remember_continuation))
  (export "initialize_compacting_gc" (func $initialize_compacting_gc))
  (export "get_reclaimed" (func $get_reclaimed))
  (export "get_total_allocations" (func $get_total_allocations))
  (export "get_heap_size" (func $get_heap_size))
  (export "alloc_blob" (func $alloc_blob))
  (export "alloc_array" (func $alloc_array))
  (export "principal_of_blob" (func $principal_of_blob))
  (export "blob_of_principal" (func $blob_of_principal))
  (export "text_of_ptr_size" (func $text_of_ptr_size))
  (export "text_concat" (func $text_concat))
  (export "blob_of_text" (func $blob_of_text))
  (export "text_singleton" (func $text_singleton))
  (export "text_iter" (func $text_iter))
  (export "text_iter_next" (func $text_iter_next))
  (export "version" (func $version))
  (export "alloc_words" (func $alloc_words))
  (export "compacting_gc" (func $compacting_gc))
  (export "schedule_compacting_gc" (func $schedule_compacting_gc))
  (export "generational_gc" (func $generational_gc))
  (export "schedule_generational_gc" (func $schedule_generational_gc))
  (export "initialize_generational_gc" (func $initialize_generational_gc))
  (export "post_write_barrier" (func $post_write_barrier))
  (export "copying_gc" (func $copying_gc))
  (export "schedule_copying_gc" (func $schedule_copying_gc))
  (export "initialize_copying_gc" (func $initialize_compacting_gc))
  (export "memcpy" (func $memcpy))
  (elem (;0;) (global.get $__table_base32) func $core::ptr::drop_in_place$LT$$RF$u32$GT$::h526868480085c5e9 $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h1ed0b47d0bd87b82)
  (data $.data (global.get $__memory_base) "/nix/store/5fczbajav40zprkh6fkcmw3yv8zgair5-rust-1.71.0-nightly-2023-04-20-8bdcc62cb/lib/rustlib/src/rust/library/core/src/iter/traits/exact_size.rscalled `Option::unwrap()` on a `None` valuebyte read out of buffersrc/char.rspeek_future_continuation: Continuation table not allocatedpeek_future_continuation: Continuation index out of rangepeek_future_continuation: Continuation index not in tablerecall_continuation: Continuation table not allocatedrecall_continuation: Continuation index out of rangerecall_continuation: Continuation index not in tableleb128_decode: overflowsrc/leb128.rssleb128_decode: overflowCannot grow memorycompute_crc32: Blob expectedsrc/principal_id.rs\00\00\00\00\00\960\07w,a\0e\ee\baQ\09\99\19\c4m\07\8f\f4jp5\a5c\e9\a3\95d\9e2\88\db\0e\a4\b8\dcy\1e\e9\d5\e0\88\d9\d2\97+L\b6\09\bd|\b1~\07-\b8\e7\91\1d\bf\90d\10\b7\1d\f2 \b0jHq\b9\f3\deA\be\84}\d4\da\1a\eb\e4\ddmQ\b5\d4\f4\c7\85\d3\83V\98l\13\c0\a8kdz\f9b\fd\ec\c9e\8aO\5c\01\14\d9l\06cc=\0f\fa\f5\0d\08\8d\c8 n;^\10iL\e4A`\d5rqg\a2\d1\e4\03<G\d4\04K\fd\85\0d\d2k\b5\0a\a5\fa\a8\b55l\98\b2B\d6\c9\bb\db@\f9\bc\ac\e3l\d82u\5c\dfE\cf\0d\d6\dcY=\d1\ab\ac0\d9&:\00\deQ\80Q\d7\c8\16a\d0\bf\b5\f4\b4!#\c4\b3V\99\95\ba\cf\0f\a5\bd\b8\9e\b8\02(\08\88\05_\b2\d9\0c\c6$\e9\0b\b1\87|o/\11LhX\ab\1da\c1=-f\b6\90A\dcv\06q\db\01\bc \d2\98*\10\d5\ef\89\85\b1q\1f\b5\b6\06\a5\e4\bf\9f3\d4\b8\e8\a2\c9\07x4\f9\00\0f\8e\a8\09\96\18\98\0e\e1\bb\0dj\7f-=m\08\97ld\91\01\5cc\e6\f4Qkkbal\1c\d80e\85N\00b\f2\ed\95\06l{\a5\01\1b\c1\f4\08\82W\c4\0f\f5\c6\d9\b0eP\e9\b7\12\ea\b8\be\8b|\88\b9\fc\df\1d\ddbI-\da\15\f3|\d3\8ceL\d4\fbXa\b2M\ceQ\b5:t\00\bc\a3\e20\bb\d4A\a5\dfJ\d7\95\d8=m\c4\d1\a4\fb\f4\d6\d3j\e9iC\fc\d9n4F\88g\ad\d0\b8`\das-\04D\e5\1d\033_L\0a\aa\c9|\0d\dd<q\05P\aaA\02'\10\10\0b\be\86 \0c\c9%\b5hW\b3\85o \09\d4f\b9\9f\e4a\ce\0e\f9\de^\98\c9\d9)\22\98\d0\b0\b4\a8\d7\c7\17=\b3Y\81\0d\b4.;\5c\bd\b7\adl\ba\c0 \83\b8\ed\b6\b3\bf\9a\0c\e2\b6\03\9a\d2\b1t9G\d5\ea\afw\d2\9d\15&\db\04\83\16\dcs\12\0bc\e3\84;d\94>jm\0d\a8Zjz\0b\cf\0e\e4\9d\ff\09\93'\ae\00\0a\b1\9e\07}D\93\0f\f0\d2\a3\08\87h\f2\01\1e\fe\c2\06i]Wb\f7\cbge\80q6l\19\e7\06knv\1b\d4\fe\e0+\d3\89Zz\da\10\ccJ\ddgo\df\b9\f9\f9\ef\be\8eC\be\b7\17\d5\8e\b0`\e8\a3\d6\d6~\93\d1\a1\c4\c2\d88R\f2\dfO\f1g\bb\d1gW\bc\a6\dd\06\b5?K6\b2H\da+\0d\d8L\1b\0a\af\f6J\036`z\04A\c3\ef`\dfU\dfg\a8\ef\8en1y\beiF\8c\b3a\cb\1a\83f\bc\a0\d2o%6\e2hR\95w\0c\cc\03G\0b\bb\b9\16\02\22/&\05U\be;\ba\c5(\0b\bd\b2\92Z\b4+\04j\b3\5c\a7\ff\d7\c21\cf\d0\b5\8b\9e\d9,\1d\ae\de[\b0\c2d\9b&\f2c\ec\9c\a3ju\0a\93m\02\a9\06\09\9c?6\0e\eb\85g\07r\13W\00\05\82J\bf\95\14z\b8\e2\ae+\b1{8\1b\b6\0c\9b\8e\d2\92\0d\be\d5\e5\b7\ef\dc|!\df\db\0b\d4\d2\d3\86B\e2\d4\f1\f8\b3\ddhn\83\da\1f\cd\16\be\81[&\b9\f6\e1w\b0owG\b7\18\e6Z\08\88pj\0f\ff\ca;\06f\5c\0b\01\11\ff\9ee\8fi\aeb\f8\d3\ffkaE\cfl\16x\e2\0a\a0\ee\d2\0d\d7T\83\04N\c2\b3\039a&g\a7\f7\16`\d0MGiI\dbwn>Jj\d1\ae\dcZ\d6\d9f\0b\df@\f0;\d87S\ae\bc\a9\c5\9e\bb\de\7f\cf\b2G\e9\ff\b50\1c\f2\bd\bd\8a\c2\ba\ca0\93\b3S\a6\a3\b4$\056\d0\ba\93\06\d7\cd)W\deT\bfg\d9#.zf\b3\b8Ja\c4\02\1bh]\94+o*7\be\0b\b4\a1\8e\0c\c3\1b\df\05Z\8d\ef\02-ABCDEFGHIJKLMNOPQRSTUVWXYZ234567accum_base32: Base32 symbol out of rangealloc_text_blob: Text too largenot implementedsrc/text.rs\00\00\00\00\00\00\00object_size: invalid object tagobject_size: forwarding pointerinvalid object tag in visit_pointer_fieldsencountered NULL object tag in visit_pointer_fieldsIDL error: RTS error: remember_continuation: Argument is not a skewed pointerassertion failed: STACK_BLOB_PTR.is_null()src/gc/generational/mark_stack.rssrc/gc/generational/remembered_set.rsassertion failed: linear_memory::LAST_HP >= ic::get_aligned_heap_base()src/gc/generational.rsassertion failed: limits.base <= limits.last_freeassertion failed: limits.last_free <= limits.free\00\09\00\00\00assertion failed: (object as usize) < self.heap.limits.baseassertion failed: pointer >= self.generation_base() as u32assertion failed: REMEMBERED_SET.as_ref().unwrap().contains(Value::from_raw(field_address as\5cn            u32))assertion failed: (location as usize) >= self.heap.limits.base &&\5cn    (location as usize) < self.heap.limits.last_freeassertion failed: self.should_be_threaded(pointed)assertion failed: self.should_be_threaded(object)assertion failed: header >= TAG_OBJECT && header <= TAG_NULLArray allocation too largeblob_of_principal: principal too shorttext_concat: Text too large0.1src/gc/generational/write_barrier.rs\00\00\00\00\00\00\00\00\94\00\00\00\00\00\00\00z\00\00\00\09\00\00\00\00\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00\01\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\d6\00\00\00\00\00\00\00\0b\00\00\00\00\00\00\00\06\00\00\00\1c\00\00\00\d6\00\00\00\00\00\00\00\0b\00\00\00\00\00\00\00\11\00\00\00\1c\00\00\00A\02\00\00\00\00\00\00\0d\00\00\00\00\00\00\00&\00\00\00 \00\00\00A\02\00\00\00\00\00\00\0d\00\00\00\00\00\00\00E\00\00\00!\00\00\00\94\02\00\00\00\00\00\00\13\00\00\00\00\00\00\00\11\00\00\00\09\00\00\00\1e\07\00\00\00\00\00\00\0b\00\00\00\00\00\00\00\da\00\00\00\05\00\00\00\1e\07\00\00\00\00\00\00\0b\00\00\00\00\00\00\00S\01\00\00\05\00\00\00B\08\00\00\00\00\00\00!\00\00\00\00\00\00\00\1a\00\00\00\05\00\00\00B\08\00\00\00\00\00\00!\00\00\00\00\00\00\002\00\00\00\05\00\00\00c\08\00\00\00\00\00\00%\00\00\00\00\00\00\00\9d\00\00\00\09\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00T\00\00\00\05\00\00\00\00\00\00\02\00\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00~\00\00\00\05\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\80\00\00\00\05\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\eb\00\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\ec\00\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\f2\00\00\004\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\01\01\00\00\09\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\02\01\00\00\09\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00;\01\00\00\12\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\009\01\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00z\01\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00{\01\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\a5\01\00\004\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\a8\01\00\00\0d\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\ee\01\00\00\09\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\f5\01\00\00\09\00\00\00\cf\08\00\00\00\00\00\00\16\00\00\00\00\00\00\00\fc\01\00\00\09\00\00\00\a4\0b\00\00\00\00\00\00$\00\00\00\00\00\00\00)\00\00\00\16\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00"))
