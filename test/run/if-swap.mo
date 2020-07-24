func wantSeeSwap() : Nat =
    if (true != true) 42 else 25

// CHECK-LABEL: wantSeeSwap
// CHECK: i32.eq
// CHECK-NEXT: if (result i32)
// CHECK-NEXT: i32.const 50
// CHECK-NEXT: else
// CHECK-NEXT: i32.const 84

