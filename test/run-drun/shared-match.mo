import Prim "mo:prim";
actor a {
  public func match_true(true) : async () {
    Prim.debugPrint "match_true";
  };
  public func go() = ignore async {
    Prim.debugPrint "go1";
    try { await match_true(true) }
    catch e {};
    try { await match_true(false) }
    catch e {};
    Prim.debugPrint "go2";
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"