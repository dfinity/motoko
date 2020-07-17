// actor references

import Prim "mo:prim";

actor a {
  public func go() : async () {

    let tests = [
      "ic:bfozs-kwa73-7nadi", // a good one
      "", // missing scheme
      "CI", // missing colon
      "https://cern.ch", // wrong scheme
      "ic:", // empty principal
      "ic:BFOZS-KWA73-7NADI", // lowercase not allowed
      "ic:bfozskwa737nadi", // missing dashes
      "ic:vpgq", // too short
      "ic:5h74t-uga73-7nadi", // wrong checksum
    ];

    for (t in tests.vals()) {
      Prim.debugPrint(debug_show t # ":");
      try (await async ignore (actor(t) : actor {}))
      catch (e) Prim.debugPrint(Prim.errorMessage(e))
    }
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
