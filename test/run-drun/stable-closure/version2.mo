import Prim "mo:prim";

// succeeds as upgrade
persistent actor {
    persistent class C<T>(x : T) {
      let a = (x, x);
      public persistent func get() : (T,T) { a }
    };
    stable let o : C<Nat> = Prim.trap "impossible";
    stable let h : Any = C(1); // why do I need this?
    assert o.get() == (0,0);
    Prim.debugPrint "version2";
}
