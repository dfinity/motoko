let _ = (16 : Nat8) * (16 : Nat8)
// There should be only one shift per operand
// CHECK: (func $mul<Nat8>
// CHECK: i64.shr_u
// CHECK: i64.const 48
// CHECK-NEXT: i64.shr_u
