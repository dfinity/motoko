import Prim "mo:prim";
actor {
  var my_state = 1;
  public query func check() : async Nat { my_state };
}
