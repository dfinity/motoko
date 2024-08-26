let _ = (16 : Int8) * (8 : Int8)
// There should be only one shift per operand
// CHECK: mul<Int8>
// CHECK: {{i32.const 24|i64.const 48}}
// CHECK-NEXT: {{i32|i64}}.shr_s
