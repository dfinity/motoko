import Prim "mo:prim";

persistent actor {
    stable func f() : {b: Int} {
        {b=1};
    };
    stable let g : stable f () -> {a:Int; b: Int} = Prim.trap "unreachable";
    assert g().a == 1;
}
