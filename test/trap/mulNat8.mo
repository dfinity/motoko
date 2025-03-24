let _ = (16 : Nat8) * (16 : Nat8)
// There should be only one shift per operand
// CHECK: (func $mul<Nat8>
// CHECK: {{i32.const 24|i64.const 48}}
// CHECK-NEXT: {{i32|i64}}.shr_u
