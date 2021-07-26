import Prim "mo:⛔";
actor this {
  Prim.debugPrint ("init'ed");
  stable var c = "a";
  public func inc() : async () { c #= "a"; };
  public query func check(n : Int) : async () {
    Prim.debugPrint(c);
    assert (c.size() == n);
  };

  var n = 0;
  var done = true;

  public func yield() : async () {
    if (not done and n < 40) {
      n += 1;
      await yield();
      n -= 1;
    }
  };

  public func repeat() : () {
    done := false;
    ignore yield();
  };

  public func wait() : async () {
    done := true;
    while (n > 0) await async {};
  };

}
