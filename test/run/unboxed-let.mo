func go1() {
 let x : Nat32 = 1 +% 1;
 assert (x *% x == 4);
};
go1();

// CHECK: func $go1
// CHECK-NOT: box_i32
// CHECK: unreachable

// Also test that the value is included in a closure properly

func goCapture() : () -> Nat32 {
 let x : Nat32 = 1 +% 1;
 return func() : Nat32 { x };
};

assert (goCapture()() == 2);
