// actor references

import Prim "mo:prim";

actor a {
  public func go() : async () {

    let tests = [
      "", // missing scheme
      "CI", // missing colon
      "https://cern.ch", // wrong scheme
      "ic:", // empty principal
      "ic:c0fefed00d41", // lowercase not allowed
      "ic:ABCDEFGH", // not hex
      "ic:C0FEFED00DE", // not even number of digits
      "ic:C0FEFED00D42", // does not validate
    ];

    for (t in tests.vals()) {
      Prim.debugPrint(t # ":");
      try (await async ignore (actor(t) : actor {}))
      catch (e) Prim.debugPrint(Prim.errorMessage(e))
    }
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
