import Prim "mo:prim";

actor a {

    let bound : Int = 100000;

    func Loop1(n : Int) {
        if (n >= bound) {
            Prim.debugPrint "Done";
            return;
        };
        Loop1(n+1);
    };

    public query func test_loop1() : async () {
        Loop1(0);
    };

    system func preupgrade() {
        Prim.debugPrint("preupgrade");
        Loop1(0);
    };

};
