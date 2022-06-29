// allocate 2 big arrays with a 10^5 entries each, and populate them
// with Int32s and Nat32s (somewhat randomly)
import Prim "mo:⛔";

actor Tagged {

    func arrNat32(seed : Nat) : ([Nat32], Nat) {
        var state = seed;
        let cutoff = 2_147_483_629;
        var taggable = 0;
        (Prim.Array_tabulate<Nat32>(
            100_000,
            func (i : Nat) {
                state := (state + 17) * 57 % cutoff;
                let bits = Prim.shiftRight(state, 30) % 4;
                if (bits == 0 or bits == 3)
                   taggable += 1;
                   //Prim.debugPrint(debug_show (Prim.intToNat32Wrap state, taggable));
                   Prim.intToNat32Wrap state
            })
         , taggable)
    };

    func arrInt32(seed : Nat) : ([Int32], Nat) {
        var state = seed;
        let cutoff = 4294967311;
        var taggable = 0;
        (Prim.Array_tabulate<Int32>(
            100_000,
            func (i : Nat) {
                state := (state + 17) * 57 % cutoff;
                let bits = Prim.shiftRight(state, 30) % 4;
                if (bits == 0 or bits == 3)
                   taggable += 1;
                   //Prim.debugPrint(debug_show (Prim.intToInt32Wrap state, taggable));
                   Prim.intToInt32Wrap state
            }), taggable)
    };

    public func go() : async () {
        let (_, nt) = arrNat32(7);
        Prim.debugPrint(debug_show nt);
        let (_, it) = arrInt32(7);
        Prim.debugPrint(debug_show it)
    }
}

//CALL ingress go 0x4449444C0000
