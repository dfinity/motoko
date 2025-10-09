import Prim "mo:prim";

persistent actor {
    persistent func f() : {a: Int; b: Int} {
        {a = 1; b = 1};
    };


    stable let g : persistent () -> {a:Int; b: Int} = Prim.trap "unreachable";
    assert g().a == 1;
    stable let h : Any = f; // why does upgrade fail without this reference to f?
    Prim.debugPrint "version1";

}
