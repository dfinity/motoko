import { error } =  "mo:⛔";

actor A {
    func m() : async () {
    };

    func _t0() : async () {
        try { await m() }
        catch _ {}
        finally { ignore m() } // BAD: no effects allowed!
    };

    func _t1() : async () {
        try { await m() }
        catch _ {}
        finally { throw error "Nope" } // BAD: has effect.
    };

    func _t2() : async () {
        try { await m() }
        catch _ {}
        finally { 42 } // BAD: should return unit.
    }

    // TODO: Nat resulting `try`
}
