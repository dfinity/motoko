import Prim "mo:⛔";

// let t = "hello";

shared actor class () {

  let t = "hello";

  Prim.debugPrint(t);

  public shared query func check () : async () {
    assert (t == "hello");
  };

  public shared func echo () : async () {
    Prim.debugPrint(t)
  };

};
