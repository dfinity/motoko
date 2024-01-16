actor {};

// The prelude should not require any code (besides maybe a call to collect) at runtime
// DON'TCHECK: (func $init (type 4))

// CHECK:  (func $@motoko_async_destabilization (type 0)
// CHECK:         call $@init_actor_after_destabilization
// CHECK:         call $post_exp
// CHECK:         call $trans_state4
// CHECK:         call $start_gc_after_upgrade
// CHECK-NEXT:    i32.const 0
// CHECK-NEXT:    call 32
// CHECK-NEXT:    global.set 5
// CHECK-NEXT:    call ${{copying_gc|compacting_gc|generational_gc|incremental_gc}}

// CHECK:  (func $canister_init
// CHECK-NEXT:    call $trans_state
// CHECK-NEXT:    call $init

// CHECK:  (export "canister_init" (func $canister_init))


