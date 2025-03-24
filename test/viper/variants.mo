actor Variants {
  public type BW = {
    #Black;
    #White;
  };

  public type Pair<A, B> = {
    #ordered : (A, B);
    #swapped : Pair<B, A>;
  };

  private func getBW(): BW {
    let x : BW = #Black;
    return x;
  };

  func idPair<A, B>(p: Pair<A, B>) : Pair<A, B> {
    return p;
  };

  func flatten<A, B>(p: Pair<A, B>) : Pair<A, B> {
    switch p {
      case (#ordered(a, b)) {
        return #ordered(a, b);
      };
      case (#swapped(sp: Pair<B, A>)) {
        switch sp {
          case (#ordered(a, b)) {
            return #ordered(b, a);
          };
          case (#swapped(sp2: Pair<A, B>)) {
            return flatten(sp2);
          };
        };
      };
    };
  };

  public func get() : async Pair<Nat, BW> {
    return idPair<Nat, BW>(#swapped(#ordered(#Black, 10)));
  };

  public func getf() : async Pair<Nat, BW> {
    return flatten<Nat, BW>(#swapped(#ordered(#Black, 10)));
  };

};
