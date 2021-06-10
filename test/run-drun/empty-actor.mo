actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// DON'TCHECK: (func $init (type 4))

// CHECK:  (func $canister_init (type 7)
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $init
// CHECK-NEXT:    call ${{copying_gc|compacting_gc}}
// CHECK-NEXT:    call $trans_state

// CHECK:  (export "canister_init" (func $canister_init))


