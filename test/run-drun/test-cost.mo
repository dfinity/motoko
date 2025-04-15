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

  // Output local replica:
  //   500_000_000_000 -- create canister cost
  //   42_102_428_000 -- costCall
  //   already topped up; balance = 2_958_887_473_523
  //   Cycles.balance()   = 2_958_887_473_523
  //   Cycles.available() = 0
  //   Cycles.refunded()  = 0
  //   created canister id: uzt4z-lp777-77774-qaabq-cai
  //   Cycles.balance()   = 2_458_881_830_595
  //   Cycles.available() = 0
  //   Cycles.refunded()  = 0
  //   500_005_642_928 -- Cycles.balance() diff
  public func go() : async () {
    print(debug_show (Prim.costCreateCanister()) # " -- create canister cost");
    print(debug_show (Prim.costCall(15, 1)) # " -- costCall");

    if (Cycles.balance() == 0) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    let before = Cycles.balance();
    printCycles();
    let { canister_id } = await (with cycles = Prim.costCreateCanister()) ic00.create_canister({ settings = null });
    let after = Cycles.balance();
    print("created canister id: " # debug_show (canister_id));
    printCycles();
    print(debug_show (before - after : Nat) # " -- Cycles.balance() diff");
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
