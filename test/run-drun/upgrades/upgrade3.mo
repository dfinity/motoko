import { debugPrint } "mo:⛔";
actor {
  debugPrint ("init'ed 3");
  stable let c : Text = do { assert false; loop {}};
  stable var i : Nat = do { assert false; loop {}};
  public func inc() { i += 1; };
  public query func check(n : Int) : async () {
    assert (c.size() == 3);
    assert (i == n);
  };
}
