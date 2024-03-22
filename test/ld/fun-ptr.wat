(module
  (type (;0;) (func (result i64)))
  (type (;1;) (func (param i32) (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (import "rts" "f2" (func $f2 (result i64)))
  (table (;0;) 1 1 funcref)
  (memory (;0;) i64 2)
  (global $heap_base i64 (i64.const 65536))
  (export "__heap_base" (global $heap_base))
  (func $call_imported (type 2)
    call $f2
    i32.wrap_i64
    call_indirect (type 0)
    i32.wrap_i64
    i32.const 3
    i32.const 5
    call_indirect (type 1)))
