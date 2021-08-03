import Prim "mo:⛔";

actor a {

  public shared func oneway_success() : () {
    Prim.debugPrint "oneway_success!"
  };

  public func go() : async () {
    oneway_success()
  }
};

await a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
