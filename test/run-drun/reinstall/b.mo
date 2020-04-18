import Prim "mo:prim";
actor {
  var my_state = 2;
  public query func check() : async Nat { my_state };
}
