func go () {
  let foobaz1 = do {
    func fuzz1() = ();
    func foobar1() = fuzz1();
    let fooquux1 = foobar1;
    fooquux1;
  };
  foobaz1();
};
go();

let foobaz2 = do {
  func fuzz2() = ();
  func foobar2() = fuzz2();
  let fooquux2 = foobar2;
  fooquux2;
};
foobaz2();

// It seems that we have to anticipate the order of these checks for FileCheck
// This means this needs to update if the compiler emits the functions in a different
// order.

// There might be a way around using CHECK-DAG, but I am not sure.

// CHECK-LABEL: (func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2

// CHECK-LABEL: (func $foobar2
// CHECK-NOT: call_indirect
// CHECK: call $fuzz2

// CHECK-LABEL: (func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

// CHECK-LABEL: (func $foobar1
// CHECK-NOT: call_indirect
// CHECK: call $fuzz1
