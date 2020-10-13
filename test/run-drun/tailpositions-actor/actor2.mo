import Prim "mo:prim";

actor a {

    let bound : Int = 100000;

    func Loop1(n : Int) {
        if (n >= bound) {
            Prim.debugPrint "done";
            return;
        };
        Loop1(n+1);
    };

    system func postupgrade() {
        Prim.debugPrint("postupgrade");
        Loop1(0);
    };

};
