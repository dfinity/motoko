// allocate 2 big arrays with a 10^5 entries each, and populate them
// with Int32s and Nat32s (somewhat randomly)
import { Array_tabulate; intToNat32Wrap; intToInt32Wrap; rts_heap_size; debugPrint } = "mo:⛔";

actor Tagged {

    func arrNat32(seed : Nat) : ([Nat32], Nat) {
        var state = seed;
        let cutoff = 2_147_483_629;
        var taggable = 0;
        (Array_tabulate<Nat32>(
            100_000,
            func _ {
                state := (state + 17) * 57 % cutoff;
                let wrapped = intToNat32Wrap state;
                let bits = (wrapped >> 30) & 3;
                if (bits == 0 or bits == 3)
                   taggable += 1;
                   wrapped
            })
         , taggable)
    };

    func arrInt32(seed : Nat) : ([Int32], Nat) {
        var state = seed;
        let cutoff = 4294967311;
        var taggable = 0;
        (Array_tabulate<Int32>(
            100_000,
            func _ {
                state := (state + 17) * 57 % cutoff;
                let wrapped = intToInt32Wrap state;
                let bits = (wrapped >> 30) & 3;
                if (bits == 0 or bits == 3)
                   taggable += 1;
                   wrapped
            }), taggable)
    };

    public func go() : async () {
        let n0 = rts_heap_size();
        let (_, nt) = arrNat32(7);
        let n1 : Int = rts_heap_size();
        debugPrint(debug_show (nt, n1 - n0));

        let i0 = rts_heap_size();
        let (_, it) = arrInt32(13);
        let i1 : Int = rts_heap_size();
        debugPrint(debug_show (it, i1 - i0))
    }
}

//CALL ingress go 0x4449444C0000
