import Prim "mo:prim";

persistent actor {
    stable func f() : {a: Int; b: Int} {
        {a = 1; b = 1};
    };

    stable let g : stable f () -> {a:Int; b: Int} = Prim.trap "unreachable";
    assert g().a == 1;
    Prim.debugPrint "version1";
}
