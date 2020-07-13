func go () {
  let { foobar1 } = module { public func foobar1() = () };
  foobar1();
};

let { foobar2 } = module { public func foobar2() = () };
foobar2();

// CHECK: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

// CHECK: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2
