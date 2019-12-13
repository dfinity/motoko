// this only works on ic-stub, where we know that we are
// ic:000000000000040054

import imported "ic:000000000000040054";
actor a {

  public func foo() : async Text { "World" };

  public func go() = ignore async {
    debugPrint("Hello " # (await imported.foo()));
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"


