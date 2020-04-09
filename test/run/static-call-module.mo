func go () {
  let obj1 = module { public func foobar1() = () };
  obj1.foobar1();
};

let obj2 = module { public func foobar2() = () };
obj2.foobar2();

// CHECK: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

// CHECK: func $start
// CHECK-NOT: call_indirect
// CHECK: call $foobar2
