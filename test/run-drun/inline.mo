//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

actor {
  func inline(t1: Text, t2 : Text) : Text = t1 # t2;
  func makecalls() : () {
    let t1 = inline("a","b"); // should be inlined
    var p = ("c","d");
    let t2 = inline p; // should not be inlined, but a still direct call
  };
  public func go() : async () {
    makecalls();
  }
}

//SKIP run
//SKIP run-ir
//SKIP run-low


// CHECK-LABEL: (func $makecalls
// CHECK-NOT: call_indirect
// CHECK: call $inline
