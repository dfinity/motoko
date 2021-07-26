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

  public func yield() : async () {};
  public func repeat() : () {
    n := 10;
    while (n > 0) {
      Prim.debugPrint(debug_show(n));
      await yield();
      n -= 1;
    };
  };

  public func wait() : async () {
    if (n > 0) await wait();
  };

}
