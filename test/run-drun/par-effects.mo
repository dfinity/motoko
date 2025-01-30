import { debugPrint } = "mo:⛔";


// test whether side-effects in parentheticals happen in the left-to-right manner

actor A {

    private func f() = debugPrint "effect";

    public func go() {
        let cycles = 888;
        await (with cycles; moot = f()) async debugPrint "async-inside";
        
    }
}

//OR-CALL ingress go "DIDL\x00\x00"
