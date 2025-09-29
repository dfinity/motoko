import Prim "mo:prim";
// ok upgrade from v1
persistent actor {
    // new definition can be a subtype
    stable func f() : Nat {
        3;
    };
    stable let g : stable f () -> Int // retained from version1.mo
      = Prim.trap "unreachable"; // unused initializer
    assert g() == 3;
    Prim.debugPrint "version3";
}
