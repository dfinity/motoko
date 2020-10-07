actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// DON'TCHECK: (func $init (type 4))

// CHECK:  (func $canister_init (type 6)
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $init
// CHECK-NEXT:    call $collect
// CHECK-NEXT:    call $trans_state

// CHECK:  (export "canister_init" (func $canister_init))


