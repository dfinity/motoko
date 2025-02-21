actor {
    func _bad(a : actor { foo : () -> async (); oneway : () -> () }) : async () {
        let defaults = { moot = 9 };
        await (with {defaults with cycles = 'C'}) a.foo();
        await (with {defaults with cycles = "Can't"}) async ();
        await (with defaults) async ();
        func nonSend() : Nat = 42;
//        ignore (with) nonSend(); syntactically incorrect
        (with cycles = 999) a.oneway(); // should not warn
//        ({} with) a.oneway(); // syntactically incorrect
        await (with timeout = 'T') a.foo();
    }
}
