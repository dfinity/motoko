import Prim "mo:prim";
//bad upgrade from v1, reject
persistent actor {
    // bad re-definition at supertype, reject!
    stable func f() : Any {
      2
    };
    stable let g : stable f () -> Int // retained from previous, at supertype
      = Prim.trap "unreachable"; // unused initializer
    assert g() == 2;
    Prim.debugPrint "version2";
}
