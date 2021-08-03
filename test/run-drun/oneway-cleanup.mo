import Prim "mo:⛔";

actor a {

  public shared func oneway_success() : () {
    Prim.debugPrint "oneway_success!"
  };

  public shared func oneway_fail() : () {
    Prim.debugPrint "oneway_fail!";
    assert false
  };

  public func go() : async () {
    oneway_success();
    await async {};
    oneway_fail()
    await async {};
  }
};

await a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
