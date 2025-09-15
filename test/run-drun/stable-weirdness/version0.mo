import Prim "mo:prim";

persistent actor {
    persistent func f() : {a : Int; b : Int} {
        {a=0; b=0};
    };

    stable let g = f;
    assert g().a == 0;

    Prim.debugPrint "version0";
}
