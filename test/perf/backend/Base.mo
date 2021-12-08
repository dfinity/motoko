/// Candids for PRs to base.
import BaseArray "mo:base/Array";

module {
  public module Array {
    // true iff array is superset of members array.
    // order does not matter.
    public func containsAll<X>(array : [X], members : [X], equal : (X, X) -> Bool) : Bool {
      for (m in members.vals()) {
        switch (BaseArray.find<X>(array, func (x : X) : Bool { equal(m, x) })) {
          case null { return false };
          case _ { };
        }
      };
      return true
    };
  }
}
