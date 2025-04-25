import Prim "mo:⛔";

actor client {
  func print(t : Text) = Prim.debugPrint("client: " # t);

  // Output local replica:
  public func go() : async () {
    print(debug_show (Prim.costCall(15, 1)) # " -- cost_call");
    print(debug_show (Prim.costCreateCanister()) # " -- cost_create_canister");
    print(debug_show (Prim.costHttpRequest(15, 2000)) # " -- cost_http_request");
    print(debug_show (Prim.costSignWithEcdsa("test_key_1", 0)) # " -- cost_sign_with_ecdsa");
    print(debug_show (Prim.costSignWithSchnorr("test_key_1", 0)) # " -- cost_sign_with_schnorr");
  };
};

client.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low