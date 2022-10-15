actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// DON'TCHECK: (func $init (type 4))

// CHECK:  (func $canister_init
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $init
// CHECK-NEXT:    i32.const 0
// CHECK-NEXT:    call 30
// CHECK-NEXT:    global.set 1
// CHECK-NEXT:    call ${{copying_gc|compacting_gc|generational_gc}}
// CHECK-NEXT:    i32.const 0
// CHECK-NEXT:    call 30
// CHECK-NEXT:    global.get 1
// CHECK-NEXT:    i64.sub
// CHECK-NEXT:    global.set 2
// CHECK-NEXT:    call $trans_state

// CHECK:  (export "canister_init" (func $canister_init))


