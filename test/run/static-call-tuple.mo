func go () {
  let (foobar1, _) = (func foobar1() { assert true; }, 5);
  foobar1();
  let tup = (func foobar1a() { assert true; }, 5);
  tup.0();
};
go();

let (foobar2, _) = (func foobar2() { assert true; }, 5);
foobar2();
let tup = (func foobar2a() { assert true; }, 5);
tup.0();

// CHECK-LABEL: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2
// CHECK: call $foobar2a

// CHECK-LABEL: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1
// CHECK: call $foobar1a
