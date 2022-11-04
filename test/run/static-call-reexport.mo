// This tests that modules can re-rexport functions and still
// get direct calls

func go () {
  let objA1 = module { public func foobar1() { assert true; } };
  let objB1 = module { public let foobar1 = objA1.foobar1; };
  objB1.foobar1();
};
go();

let objA2 = module { public func foobar2() { assert true; } };
let objB2 = module { public let foobar2 = objA2.foobar2; };
objB2.foobar2();

// CHECK-LABEL: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2

// CHECK-LABEL: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1
