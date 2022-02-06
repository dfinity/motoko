// test cycle overflow detection and larger cycle transfers

import Prim "mo:⛔";

actor a {

  public func go() : async (){
    // works for 0x1_0000000_000000;
    // works for 0xF000000_000000;
    let amount = 0xFFFFFFFF_00000000;

    Prim.debugPrint("topping up");
    let ic00 = actor "aaaaa-aa" : actor {
      provisional_top_up_canister :
        { canister_id: Principal; amount : Nat } -> async ();
    };
    await ic00.provisional_top_up_canister({
      canister_id = Prim.principalOfActor(a);
      amount = amount});
    Prim.debugPrint("done");
  };
};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"
//SKIP run
//SKIP run-ir
//SKIP run-low

