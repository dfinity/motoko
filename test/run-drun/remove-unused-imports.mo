import Prim "mo:prim";

actor {
  public func print() : async () {
    Prim.debugPrint("print");
  };
};

// CHECK-NOT: import "ic0" "root_key_copy"
// CHECK-NOT: import "ic0" "root_key_size"
// CHECK: import "ic0" "debug_print"
// CHECK-NOT: import "ic0" "root_key_copy"
// CHECK-NOT: import "ic0" "root_key_size"
