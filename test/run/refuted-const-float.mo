// a failing pattern match that can be compiled to a trap
let 0.67 = 3.14;

// CHECK: (func $init (type
// CHECK: call $blob_of_principal
// CHECK: i64.const 14
// CHECK: call $print_address_length
// CHECK-NEXT: unreachable)

//SKIP run-low
//SKIP run-ir
