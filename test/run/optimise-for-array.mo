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

let array = [var "hello", "remutable", "world"];
array[1] := "mutable";
// CHECK:      call $@mut_array_size
// CHECK:      call $B_lt
// CHECK:      local.get $array
// CHECK:      local.set $check2
// `arr` being a `VarE` already
for (check2 in array.vals()) { Prim.debugPrint check2 };

// CHECK:      call $@immut_array_size
// CHECK:      call $B_lt
// CHECK:      call $Array.idx_bigint
// CHECK:      local.set $check3
// interfering parentheses don't disturb us
for (check3 in (((["hello", "immutable", "world"].vals())))) { Prim.debugPrint check3 };
