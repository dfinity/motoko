import Prim "mo:⛔";

//let t = "wrong"; // test capture avoidance

actor class (t : Text, t1 : Text) {

  Prim.debugPrint(t);

  public shared query func check () : async () {
    assert (t == "Hello" and t1 == "World");
  };

  public shared func echo () : async () {
    Prim.debugPrint(t);
    Prim.debugPrint(t1)
  };

};
