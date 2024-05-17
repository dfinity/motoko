actor Variants {
  public type BW = {
    #Black;
    #White;
  };

  public type Pair<A, B> = {
    #ordered : (A, B);
    #swapped : Pair<B, A>;
  };

  public func get() : async Pair<Nat, BW> {
    return #swapped(#ordered(#Black, 10));
  }

};
