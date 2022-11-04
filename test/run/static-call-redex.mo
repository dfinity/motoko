func go () {
  (func foobar1() { assert true; })();
};
go();

(func foobar2() { assert true; })();

// CHECK-LABEL: func $init
// CHECK-NOT: call_indirect
// CHECK: call $foobar2

// CHECK-LABEL: func $go
// CHECK-NOT: call_indirect
// CHECK: call $foobar1
