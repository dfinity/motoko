actor Tuple {

    let fld1 = (1, false);
    var fld2 = (1, 2);

    public func localTuple(): async () {
        let t = (1, 2) : (Int, Int);
        let a = t.0;
        let b = t.1;
        let t1 = (a, b);
        assert t == t1;
        var t2 = (1, 2): (Int, Int);
        t2 := (3, 4);
        // assert t2 != (1, 2);
        assert t2.0 != 1 and t2.1 != 2;
        // assert t2 == (3, 4);
        assert t2.0 == 3 and t2.1 == 4;
    };

    private func getTuple(): (Int, Bool) {
        return (42, false);
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
        assert t.0 == 42 and t.1 == false;
        let (a, b) = t;
        assert a == 42 and b == false;
    };

    public func changeField(): async () {
        let x = fld1.0;
        fld2 := (2, 3);
    }
}
