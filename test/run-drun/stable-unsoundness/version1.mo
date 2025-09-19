import Prim "mo:prim";
// ok upgrade from version 0
persistent actor {
    // redefinition at same type
    stable func f() : Int {
        1
    };

    stable let g : stable f () -> Int = // retained from version 0
      Prim.trap "unreachable";
    assert g() == 1;
    Prim.debugPrint "version1";
}
