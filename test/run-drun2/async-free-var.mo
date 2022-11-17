import Prim "mo:⛔";
actor a {
  public func go() : async () {
    var x = "A";
    Prim.debugPrint x;
    let a = async {
      Prim.debugPrint "Now in async";
      Prim.debugPrint x;
      x := "B";
      Prim.debugPrint x;
    };
    Prim.debugPrint x;
    x := "C";
    Prim.debugPrint x;
    await a;
    Prim.debugPrint x;
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
