func go() {
 let x : Nat32 = 1 +% 1;
 assert (x *% x == 4);
};

// CHECK: func $go
// CHECK-NOT: box_i32
// CHECK: unreachable

go();
