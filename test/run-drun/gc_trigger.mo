import Prim "mo:prim";

actor {
    let retained = Prim.Array_init<Nat>(8 * 1024 * 1024, 0);
    ignore Prim.Array_init<Nat>(8 * 1024 * 1024, 0); // garbage
};

//CALL ingress __motoko_gc_trigger "DIDL\x00\x00"
//CALL ingress __motoko_gc_trigger "DIDL\x00\x00"

