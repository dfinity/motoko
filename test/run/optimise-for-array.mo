import Prim "mo:⛔";

// CHECK: (local $check0 i32)
// CHECK: call $B_lt
// CHECK: call $Array.idx_bigint
// CHECK: local.set $check0
// CHECK: local.get $check0
// CHECK-NEXT: call $debugPrint
for (check0 in ["hello", "world"].vals()) { Prim.debugPrint check0 };

for (s in [var "hello", "mutable", "world"].vals()) { Prim.debugPrint s };

let arr = ["hello", "immutable", "world"];
for (s in arr.vals()) { Prim.debugPrint s };
