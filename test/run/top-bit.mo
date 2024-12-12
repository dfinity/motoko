//CLASSICAL-PERSISTENCE-ONLY

var x : Nat32 = 4294967295;
assert (x >> 31 != 0);
assert (x & 2147483648 != 0);

// CHECK-LABEL: (func $init
// CHECK: i32.clz
// CHECK-NEXT: if  ;;
// CHECK: i32.clz
// CHECK-NEXT: if  ;;
// CHECK: end)
