import Prim "mo:⛔";

import Cycles "cycles/cycles";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  type Key = { name : Text; curve : EcdsaCurve };
  type EcdsaCurve = { #secp256k1 };
  type Args = {
    key_id : Key;
    derivation_path : [[Nat8]];
    message_hash : [Nat8];
  };
  let ic00 = actor "aaaaa-aa" : actor {
    sign_with_ecdsa : shared Args -> async { signature : [Nat8] };
  };
  func encodeCurve(curve : EcdsaCurve) : Nat32 = switch curve {
    case (#secp256k1) 0;
  };

  public shared ({ caller }) func go() : async () {
    let fakeMessageHash : [Nat8] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32];
    let key : Key = { name = "test_key_1"; curve = #secp256k1 };
    let args : Args = {
      message_hash = fakeMessageHash;
      derivation_path = [Prim.blobToArray(Prim.blobOfPrincipal(caller))];
      key_id = key;
    };

    // Local Output:
    // client: test args:
    // {derivation_path = [[191, 235, 175, 59, 48, 178, 170, 114, 99, 211, 183, 152, 77, 161, 198, 129, 22, 97, 67, 213, 112, 208, 58, 98, 224, 28, 43, 71, 2]]; key_id = {curve = #secp256k1; name = "test_key_1"}; message_hash = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]}
    // client: (0, 26_153_846_153) -- sign with ecdsa cost
    // client: Cycles.balance()   = 2_916_762_209_195
    // client: Cycles.available() = 0
    // client: Cycles.refunded()  = 0
    // client: signature: [1, 74, 96, 177, 200, 15, 166, 139, 132, 158, 253, 124, 73, 75, 101, 0, 70, 109, 150, 128, 92, 190, 37, 129, 251, 243, 179, 15, 84, 174, 166, 188, 23, 59, 224, 149, 183, 58, 233, 112, 62, 133, 63, 108, 135, 100, 227, 31, 92, 166, 7, 219, 83, 115, 95, 41, 116, 101, 205, 127, 15, 214, 255, 6]
    // client: Cycles.balance()   = 2_890_602_540_395
    // client: Cycles.available() = 0
    // client: Cycles.refunded()  = 0
    // client: 26_159_668_800 -- Cycles.balance() diff
    // client: error message: "sign_with_ecdsa request sent with 26_153_846_152 cycles, but 26_153_846_153 cycles are required."
    await test(args);
  };

  func test(args : Args) : async () {
    print("test args:\n" # debug_show args);
    let (code, cost) = Prim.costSignWithEcdsa(args.key_id.name, encodeCurve(args.key_id.curve));
    print(debug_show (code, cost) # " -- sign with ecdsa cost");
    assert code == 0 and cost > 0;

    let before = Cycles.balance();
    printCycles();
    let { signature } = await (with cycles = cost) ic00.sign_with_ecdsa(args);
    let after = Cycles.balance();
    print("signature: " # debug_show (signature));
    printCycles();
    print(debug_show (before - after : Nat) # " -- Cycles.balance() diff");

    // Try the same args with less cycles, it should fail
    try {
      let _ = await (with cycles = cost - 1) ic00.sign_with_ecdsa(args);
      assert false; // Should not happen
    } catch (e) {
      assert Prim.errorCode(e) == #canister_reject;
      print("error message: " # debug_show (Prim.errorMessage(e)));
    };
    print("---");
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
