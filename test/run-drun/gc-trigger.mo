//MOC-NO-FORCE-GC
//INCREMENTAL-GC-ONLY
import Prim "mo:prim";

actor {
    let retained = Prim.Array_init<Nat>(6 * 1024 * 1024, 0);
    // GC is triggered during initialization

    var heapSizeWithGarbage = 0;

    public func createGarbage(): async() {
        ignore Prim.Array_init<Nat>(1024 * 1024, 0);
        ignore Prim.Array_init<Nat>(1024 * 1024, 0);
        heapSizeWithGarbage := Prim.rts_heap_size();
        // Growth is too little to trigger the incremental GC.
        // Note: The generational GC would still run the GC.
    };

    public query func checkBeforeGC(): async() {
        assert(Prim.rts_heap_size() >= heapSizeWithGarbage);
    };

    public query func checkAfterGC(): async() {
        assert(retained.size() > 0); // ensures that the array is not collected
        assert(Prim.rts_heap_size() < heapSizeWithGarbage);
    };
};

//CALL ingress createGarbage "DIDL\x00\x00"
//CALL query checkBeforeGC "DIDL\x00\x00"
//CALL ingress __motoko_gc_trigger "DIDL\x00\x00"
//CALL ingress __motoko_gc_trigger "DIDL\x00\x00"
//CALL query checkAfterGC "DIDL\x00\x00"
