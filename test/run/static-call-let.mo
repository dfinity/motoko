func go () {
  let foobar1 = func foobar1() = ();
  foobar1();
};

let foobar2 = func foobar2() = ();
foobar2();

// CHECK: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

// CHECK: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2
