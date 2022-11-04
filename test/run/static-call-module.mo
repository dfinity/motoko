func go () {
  let obj1 = module { public func foobar1() { assert true; } };
  obj1.foobar1();
};
go();

let obj2 = module { public func foobar2() { assert true; } };
obj2.foobar2();

// CHECK-LABEL: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2

// CHECK-LABEL: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1
