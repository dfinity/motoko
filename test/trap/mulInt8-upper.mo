let _ = (16 : Int8) * (8 : Int8)
// There should be only one shift per operand
// CHECK: mul<Int8>
// CHECK: i64.const 48
// CHECK-NEXT: i64.shr_s
