import Prim "mo:prim";

actor a {

    let bound : Int = 100000;

    func Loop1(n : Int) {
        if (n >= bound) {
            Prim.debugPrint "done 1";
            return;
        };
        Loop1(n+1);
    };

    public func test_loop1() : async () {
        Loop1(0);
    };

};

a.test_loop1(); //OR-CALL ingress test_loop1 "DIDL\x00\x00"
//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
