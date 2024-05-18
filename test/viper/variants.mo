actor Variants {
  public type BW = {
    #Black;
    #White;
  };

  public type Pair<A, B> = {
    #ordered : (A, B);
    #swapped : Pair<B, A>;
  };

  func idPair<A, B>(p: Pair<A, B>) : Pair<A, B> {
    return p;
  };

  public func get() : async Pair<Nat, BW> {
    return idPair<Nat, BW>(#swapped(#ordered(#Black, 10)));
  }

};
