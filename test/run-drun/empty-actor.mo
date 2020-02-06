actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// CHECK: (func $start (type 4))

// CHECK:  (func $start_stub (type 4)
// CHECK-NEXT:    call $start
// CHECK-NEXT:    call $collect)

// CHECK:  (export "canister_init" (func $start_stub))


