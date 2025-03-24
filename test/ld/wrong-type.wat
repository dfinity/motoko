(module
  (type (;0;) (func))
  (import "rts" "exported" (func $exported (param i32)))
  (table (;0;) 0 0 funcref)
  (memory (;0;) i64 2)
  (global $heap_base i64 (i64.const 65536))
  (export "__heap_base" (global $heap_base))
  (func $call_imported (type 0)
    i32.const 42
    call $exported
  )
)
