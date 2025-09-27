import Prim "mo:prim";

persistent actor {
    stable func f() : Int {
        0
    };

    stable let g = f;
    assert g() == 0;

    Prim.debugPrint "version0";
}
