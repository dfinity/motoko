import Prim "mo:prim";

persistent actor {
    persistent class C<T>(x : T) {
      let a = (x, x);
      public persistent func get() : (T,T) { a }
    };
    stable let o = C(0);
    Prim.debugPrint "version0";
}
