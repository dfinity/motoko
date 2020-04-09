func go () {
  (func foobar1() = ())();
};

(func foobar2() = ())();

// CHECK: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1

// CHECK: func $start
// CHECK-NOT: call_indirect
// CHECK: call $foobar2
