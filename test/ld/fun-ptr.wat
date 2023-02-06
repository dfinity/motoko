(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (param i32) (result i32)))
  (import "rts" "f2" (func $f2 (result i32)))
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global $heap_base i32 (i32.const 65536))
  (export "__heap_base" (global $heap_base))
  (func $call_imported (type 0)
    call $f2
    call_indirect (type 0)
    i32.const 3
    i32.const 5
    call_indirect (type 1)))
