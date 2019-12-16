import imported "ic:000000000000040054";
actor a {

  public func foo(a : actor {}) : async Text { "World" };

  public func go() = ignore async {
    debugPrint("Hello " # (await foo(imported)));
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"


