let _ = (16 : Nat8) * (16 : Nat8)
// There should be only one shift per operand
// CHECK: (func $mul<Nat8>
// CHECK: i32.shr_u
// CHECK: i32.const 24
// CHECK-NEXT: i32.shr_u
