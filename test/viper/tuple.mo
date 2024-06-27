// @verify

import Prim "mo:⛔";

actor Tuple {

    let fld1 = (1, false);
    var fld2 = (1, 2);

    public func localTuple(): async () {
        let t = (1, 2) : (Int, Int);
        let a = t.0;
        let b = t.1;
        let t1 = (a, b);
        assert:system t.0 == t1.0 and t.1 == t1.1;
        var t2 = (1, 2): (Int, Int);
        t2 := (3, 4);
        // assert:system t2 != (1, 2);
        assert:system t2.0 != 1 and t2.1 != 2;
        // assert:system t2 == (3, 4);
        assert:system t2.0 == 3 and t2.1 == 4;
    };

    private func getTuple(): (Int, Bool) {
        assert:return Prim.Ret<(Int, Bool)>().0 == 42 and Prim.Ret<(Int, Bool)>().1 == false;
        return (42, false);
    };

    private func getLargeTuple():
        (Int, Bool, Int, Int, Bool,
         Int, Bool, Int, Bool, (Int, Bool))
    {
        let t = getTuple();
        return (1, true, 2, 3, false,
                4, true, 5, false, t);
    };

    private func tupleArg(a: (Bool, Bool)): Bool {
        return a.0 and a.1;
    };

    private func tupleArg2(a: (Int, Int), b: Int) : Int {
        return a.0 + a.1 + b;
    };

    private func passTuple(): () {
        // let r = tupleArg((true, false)); // will be translated into:
        let t = (true, false);
        let r = tupleArg(t);

        // let r2 = tupleArg2((1, 2), 2);
        let t1 = (1, 2);
        let r2 = tupleArg2(t1, 2);
    };

    private func callTuple(): () {
        let t = getTuple();
        assert:system t.0 == 42 and t.1 == false;
        let (a, b) = t;
        assert:system a == 42 and b == false;
    };

    public func changeField(): async () {
        let x = fld1.0;
        fld2 := (2, 3);
    }
}
