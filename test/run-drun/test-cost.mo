import Prim "mo:⛔";

import Cycles "cycles/cycles";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  let ic00 = actor "aaaaa-aa" : actor {
    create_canister : {
      settings : ?{
        controllers : ?[Principal];
        compute_allocation : ?Nat;
        memory_allocation : ?Nat;
        freezing_threshold : ?Nat;
      };
    } -> async { canister_id : Principal };
  };

  public func go() : async () {
    print(debug_show (Prim.costCreateCanister()) # " -- create canister cost");

    let before0 = Cycles.balance();
    let before1 = Cycles.balance();
    let { canister_id } = await ic00.create_canister({ settings = null });
    let after = Cycles.balance();
    print("created canister id: " # debug_show (canister_id));
    print(debug_show(before0) # " -- Cycles.balance() before0");
    print(debug_show(before1) # " -- Cycles.balance() before1");
    print(debug_show(after) # " -- Cycles.balance() after");
    print(debug_show (before1 - after : Nat) # " -- Cycles.balance() diff");
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
