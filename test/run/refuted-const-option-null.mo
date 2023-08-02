// a failing pattern match that can be compiled to a trap
let ?b = null;

// CHECK: (func $init (type
// CHECK: call $blob_of_principal
// CHECK: i32.const 14
// CHECK-NEXT: call $print_ptr
// CHECK-NEXT: unreachable)

//SKIP run-low
//SKIP run-ir
