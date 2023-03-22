import Prim "mo:⛔";
actor a {
  public func ping() : async () {
  };

  public func go() : async () {
    let length = 4 * 1024;
    await ping();
    let s0 = Prim.rts_heap_size();
    ignore Prim.Array_init<()>(length, ());
    // discard big array
    // Do not install in variable as it is otherwise
    // put to the remembered set for young generation
    // and it would then need a full incremental GC to 
    // reclaim.
    await ping();
    let s1 = Prim.rts_heap_size();
    let b = Prim.Array_init<()>(length, ());
    // retain b with subsequent access to it
    await ping();
    let s2 = Prim.rts_heap_size();
    // ensure that b is retained
    assert(b.size() == length);

    Prim.debugPrint(
      "Ignore Diff: " #
      debug_show s0 # " " #
      debug_show s1 # " " #
      debug_show s2 # " "
    );
    // Checks that GC correctly discards or retains the arrays (16 KB).
    // Using --forced-gc and allowing young collection for generational GC.
    // It allows for some wiggle room
    let reserve = 1 * 1024;
    assert (+s1-s0 < reserve);
    assert (+s2-s0 + reserve > 4 * length);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"


//SKIP run
//SKIP run-low
//SKIP run-ir
