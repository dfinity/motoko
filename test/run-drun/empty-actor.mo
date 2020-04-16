actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// CHECK: (func $start (type 4))

// CHECK:  (func $canister_init (type 4)
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $start
// CHECK-NEXT:    call $collect
// CHECK-NEXT:    call $trans_state

// CHECK:  (export "canister_init" (func $canister_init))


