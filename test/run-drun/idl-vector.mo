import Prim "mo:⛔";

actor {
  public type Rose = [Rose];
  public type MayRose = ?[MayRose];

  func may(r : Rose) : MayRose =
    ?Prim.Array_tabulate<MayRose>(r.size(), func (i : Nat) = may(r[i]));

  public query func rose(r : Rose) : async MayRose {
    may r
  }
}

// [[], [[], [[], [],], [], []], []]
//CALL query rose 0x4449444c016d00010003000400020000000000

//SKIP run
//SKIP run-ir
//SKIP run-low
