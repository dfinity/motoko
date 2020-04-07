let FOO = 1;
func go (x:Nat) : Nat {
  func rec(x:Nat) : Nat {
    if (x == 0) {
      return 0;
    } else {
      return 1 + rec(x-1); // not tail recursive, we dont want tailrec to kick in
    };
  };
  return rec(x);
};
assert (go(1000) == 1000);

// CHECK: func $go
// CHECK-NOT: call_indirect
// CHECK: call $rec

// CHECK: func $rec
// CHECK-NOT: call_indirect
// CHECK: call $rec

