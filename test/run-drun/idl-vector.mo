type Rose = [Rose];
type MayRose = ?[MayRose];

func may(r : Rose) : MayRose = ?Array_tabulate<MayRose>(r.len(), func (i : Nat) = may(r[i]));

actor {
  public query func rose(r : Rose) : future MayRose {
    may r
  }
}

// [[], [[], [[], [],], [], []], []]
//CALL query rose 0x4449444c016d00010003000400020000000000
