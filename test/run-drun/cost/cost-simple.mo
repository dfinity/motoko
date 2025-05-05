import Prim "mo:⛔";

persistent actor {
  public func test() : async () {
    Prim.debugPrint(debug_show (Prim.costCall(15, 1)) # " -- cost_call");
    Prim.debugPrint(debug_show (Prim.costCreateCanister()) # " -- cost_create_canister");
    Prim.debugPrint(debug_show (Prim.costHttpRequest(15, 2000)) # " -- cost_http_request");

    Prim.debugPrint(debug_show (Prim.costSignWithEcdsa("wrong_key", 0)) # " -- cost_sign_with_ecdsa");
    Prim.debugPrint(debug_show (Prim.costSignWithSchnorr("wrong_key", 0)) # " -- cost_sign_with_schnorr");

    // Future work: Operation below do not work as expected in drun. Fix them once PocketIC is the new runner.
    let validKey = "test_key_1"; // This is a valid key, but drun has an empty set of keys.
    let validCurveOrAlgorithm: Nat32 = 0; // Valid argument for both ecdsa and schnorr.
    let invalidCurveOrAlgorithm: Nat32 = 42; // Just a big number which is impossible to be valid.
    Prim.debugPrint(debug_show (Prim.costSignWithEcdsa(validKey, invalidCurveOrAlgorithm)) # " -- cost_sign_with_ecdsa"); // todo: wrong result code
    Prim.debugPrint(debug_show (Prim.costSignWithSchnorr(validKey, invalidCurveOrAlgorithm)) # " -- cost_sign_with_schnorr"); // todo: wrong result code

    Prim.debugPrint(debug_show (Prim.costSignWithEcdsa(validKey, validCurveOrAlgorithm)) # " -- cost_sign_with_ecdsa");
    Prim.debugPrint(debug_show (Prim.costSignWithSchnorr(validKey, validCurveOrAlgorithm)) # " -- cost_sign_with_schnorr");
  };
};
