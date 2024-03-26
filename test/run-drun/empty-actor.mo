actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// DON'TCHECK: (func $init (type 4))

// CHECK:  (func $@motoko_async_destabilization (type 0)
// CHECK:         call $trans_state10 
// CHECK:         call $post_exp
// CHECK:         call $start_gc_after_destabilization
// CHECK:         call $trans_state4
// CHECK:         call $@initialize_main_actor

// CHECK:  (func $canister_init
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $init

// CHECK:  (export "canister_init" (func $canister_init))
