//CLASSICAL-PERSISTENCE-ONLY
actor {};

// CHECK:  (func $canister_init
// CHECK:    call ${{copying_gc|compacting_gc|generational_gc|incremental_gc}}

// CHECK:  (export "canister_init" (func $canister_init))
