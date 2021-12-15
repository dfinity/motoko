import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Nat32 "mo:base/Nat32";
import Debug "mo:base/Debug";

module {
  /// (Infinite) streams (no ending).
  public type Stream<X> = { next : () -> X };

  /// Transform infinite iterator
  public func fromIter<X>(iter : Iter.Iter<X>) : Stream<X> {
    object {
      public func next() : X {
        switch (iter.next()) {
        case null { assert false; loop { } };
        case (?x) { x }
        }
      }
    }
  };

  /// Stream of numbers drawn from a [Bernoulli_distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
  public module Bernoulli {
    public type Value = Nat32;
    public func seedFrom(seed : Nat) : Stream<Value> {
      object {
        func hash() : Nat32 {
          Hash.hash(nextNum + 1); // avoid zero (hash is also zero)
        };
        var nextNum : Nat = seed;
        var nextHash : Nat32 = hash();
        public func next() : Value {
          let level = Nat32.bitcountTrailingZero(nextHash);
          nextNum := nextNum + 1;
          nextHash := hash();
          Nat32.fromNat(Nat32.toNat(level));
        };
      }
    }
  };
}
