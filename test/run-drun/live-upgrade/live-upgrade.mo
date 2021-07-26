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
    Prim.debugPrint(debug_show(n));
    while (n > 0) {
      await yield();
      n -= 1;
    };
    Prim.debugPrint(debug_show(n));
  };

  public func wait() : async () {
    if (n > 0) await wait();
  };

}
