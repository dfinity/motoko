import Prim "mo:prim";

persistent actor {
    persistent func f() : {b: Int} {
        {b=1};
    };
    stable let g : persistent () -> {a:Int; b: Int} = Prim.trap "unreachable";
    assert g().a == 1;
    stable let h : Any = f;
}
