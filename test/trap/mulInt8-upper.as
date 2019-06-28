let _ = (16 : Int8) * (8 : Int8)
// CHECK: mul<Int8>
// CHECK: i32.shr_s
// CHECK: i32.const 16
// CHECK-NEXT: i32.shr_s
// CHECK-NEXT: i32.const 8
// CHECK-NEXT: i32.shr_s
