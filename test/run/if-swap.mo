func wantSeeSwap(x : Bool) : Nat =
    if (not x) 42 else 25;

ignore(wantSeeSwap(true));

// CHECK-LABEL: (func $wantSeeSwap
// CHECK-NEXT: local.get $x
// CHECK-NEXT: if (result i32)
// CHECK-NEXT: i32.const 50
// CHECK-NEXT: else
// CHECK-NEXT: i32.const 84

