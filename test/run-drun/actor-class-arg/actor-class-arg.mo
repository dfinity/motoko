import Prim "mo:⛔";

// let t = "wrong"; // test capture avoidance

shared actor class (t : Text) {

  Prim.debugPrint(t);

  public shared query func check () : async () {
    assert (t == "Hello");
  };

  public shared func echo () : async () {
    Prim.debugPrint(t)
  };

};
