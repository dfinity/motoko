func go () {
  func foobar1() = ();
  let foobaz1 = foobar1;
  foobaz1();
};
go();

func foobar2() = ();
let foobaz2 = foobar2;
foobaz2();

// CHECK-LABEL: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2

// CHECK-LABEL: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

