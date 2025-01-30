import { debugPrint } = "mo:⛔";


// test whether side-effects in parentheticals happen in the left-to-right manner

actor A {

    public func outcall() : async () {
        debugPrint "outcall-inside"
    };

    private func selfcall() : async () {
        debugPrint "selfcall-inside"
    };

    private func f() = debugPrint "effect";

    public func go() {
        let cycles = 888;
        await (with cycles; moot = f()) async debugPrint "async-inside";

        await (with cycles; moot = f()) A.outcall();
        await (with cycles; moot = f()) outcall();

        await (with cycles; moot = f()) selfcall();
    }
}

//OR-CALL ingress go "DIDL\x00\x00"
