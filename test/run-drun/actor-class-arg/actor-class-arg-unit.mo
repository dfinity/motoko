import Prim "mo:prim";

let t = "hello";

shared actor class () {

  Prim.debugPrint(t);

  public shared query func check () : async () {
    assert (t == "hello");
  };

  public shared func echo () : async () {
    Prim.debugPrint(t)
  };

};
