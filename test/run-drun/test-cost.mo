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

    if (Cycles.balance() == 0) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    // let before = Cycles.balance();
    printCycles();
    let { canister_id } = await (with cycles = 500_000_000_000) ic00.create_canister({ settings = null });
    // let after = Cycles.balance();
    print("created canister id: " # debug_show (canister_id));
    printCycles();

    // print(debug_show (before) # " -- Cycles.balance() before");
    // print(debug_show (after) # " -- Cycles.balance() after");
    // print(debug_show (before - after : Nat) # " -- Cycles.balance() diff");
  };

  func printCycles() {
    print("Cycles.balance()   = " # debug_show (Cycles.balance()));
    print("Cycles.available() = " # debug_show (Cycles.available()));
    print("Cycles.refunded()  = " # debug_show (Cycles.refunded()));
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
