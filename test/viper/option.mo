actor Option {

    let fld1: ?Int = null;
    var fld2: ?Bool = ?true;

    public func localOption(): async () {
        let t1 = null : ?Int;
        let t2 = ?42 : ?Int;
        var t3 = null : ?Int;
        var t4 = ?32 : ?Int;

        // let a1 = switch t1 { case null 0; case (?x) x};
        // let a2 = switch t1 { case null null; case (?x) (?(x + 1))};
        var a2 : Int = 0;
        switch t1 {
            case null a2 := 0;
            case (?x) a2 := x;
        }
    };

    private func getOption(): ?Bool {
        return ?false;
    };

    private func takeOption(a: ?Int): Int {
        switch a {
            case null { return 0 };
            case (?x) { return x };
        }
    };

     private func passOption(): () {
        let a2 = takeOption(null);
    };

    private func callTuple(): () {
        let x = getOption();
    };

    public func changeField(): async () {
        fld2 := null;
    }
}
