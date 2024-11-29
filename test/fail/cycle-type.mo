actor {
    func _bad(a : actor { foo : () -> async () }) : async () {
        let defaults = { moot = 9 };
        await (defaults with cycles = 'C') a.foo();
        await (defaults with cycles = "Can't") async ();
        func nonSend() : async Nat = async 42;
        ignore await (defaults with cycles = 0) nonSend();
    }
}
