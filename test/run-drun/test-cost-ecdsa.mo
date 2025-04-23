import Prim "mo:⛔";

import Cycles "cycles/cycles";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  type Key = { name : Text; curve : ecdsa_curve };
  type ecdsa_curve = { #secp256k1 };
  let ic00 = actor "aaaaa-aa" : actor {
    sign_with_ecdsa : shared {
      key_id : Key;
      derivation_path : [[Nat8]];
      message_hash : [Nat8];
    } -> async { signature : [Nat8] };
  };

  public shared ({ caller }) func go() : async () {
    let fakeMessageHash : [Nat8] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32];
    let key : Key = { name = "test_key_1"; curve = #secp256k1 };
    print(debug_show (Prim.costSignWithEcdsa(key.name, key.curve)) # " -- sign with ecdsa cost");

    if (Cycles.balance() == 0) {
      await Cycles.provisional_top_up_actor(client, 3_000_000_000_000);
      print("top up; balance = " # debug_show (Cycles.balance()));
    } else {
      print("already topped up; balance = " # debug_show (Cycles.balance()));
    };

    let before = Cycles.balance();
    printCycles();
    let { signature } = await (with cycles = 500_000_000_000) ic00.sign_with_ecdsa({
      message_hash = fakeMessageHash;
      derivation_path = [Prim.blobToArray(Prim.blobOfPrincipal(caller))];
      key_id = key;
    });
    let after = Cycles.balance();
    print("signature: " # debug_show (signature));
    printCycles();

    print(debug_show (before) # " -- Cycles.balance() before");
    print(debug_show (after) # " -- Cycles.balance() after");
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
