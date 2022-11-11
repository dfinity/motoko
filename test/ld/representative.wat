(module
  (type (;0;) (func))
  (import "rts" "not_yet_imported1" (func $not_yet_imported1))
  (import "rts" "square" (func $square (param i32) (result i32)))
  (import "rts" "not_yet_imported2" (func $not_yet_imported2))
  (table (;0;) 0 0 funcref)
  (memory (;0;) 2)
  (global $heap_base i32 (i32.const 65536))
  (export "__heap_base" (global $heap_base))
  (export "resolved_import" (func $resolved_export))
  (func $call_imported (type 0)
    i32.const 42
    call $square
    drop
    call $not_yet_imported1
    call $not_yet_imported2
  )
  (func $resolved_export (type 0))
)
