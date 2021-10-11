import Prim "mo:⛔";

// CHECK: (local $check0 i32)

// CHECK:      call $@immut_array_size
// CHECK:      call $B_lt
// CHECK:      call $Array.idx_bigint
// CHECK:      local.set $check0
// CHECK:      local.get $check0
// CHECK-NEXT: call $debugPrint
// CHECK:      i32.const 2
// CHECK-NEXT: call $B_add
for (check0 in ["hello", "world"].vals()) { Prim.debugPrint check0 };

// CHECK:      call $@mut_array_size
// CHECK:      call $B_lt
// CHECK:      call $Array.idx_bigint
// CHECK:      local.set $check1
// CHECK:      local.get $check1
// CHECK-NEXT: call $debugPrint
for (check1 in [var "hello", "mutable", "world"].vals()) { Prim.debugPrint check1 };

let arr = ["hello", "immutable", "world"];
for (s in arr.vals()) { Prim.debugPrint s };
