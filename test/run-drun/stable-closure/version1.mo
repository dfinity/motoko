import Prim "mo:prim";

// fails as upgrade
persistent actor {
    persistent class C<T>(x : T) {
      let a = (x, x);
      public persistent func get() : (T,T) { a }
    };
    stable let o : C<Nat> = Prim.trap "impossible";
    assert o.get() == (0,0);
    Prim.debugPrint "version1";
}
