import Prim = "mo:⛔";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  public func go() : async () {
    print("create canister: " # debug_show (Prim.costCreateCanister()));
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
