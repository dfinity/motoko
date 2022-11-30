import Prim "mo:⛔";
actor a {
  public func ping() : async () {
  };

  public func go() : async () {
    let length = 1024 * 1024;
    // Using reclaimed size as the incremental GC uses free lists and does not reduce the heap size.
    let r0 = Prim.rts_reclaimed();
    var a = Prim.Array_init<()>(length, ());
    // discard bigger array a
    a := Prim.Array_init<()>(0, ());
    await ping(); // need here 2 GC increments for incremental GC to free garbage
    await ping();
    let r1 = Prim.rts_reclaimed();
    let b = Prim.Array_init<()>(length, ());
    // retain b with subsequent access to it
    await ping(); // 2 GC increments for incremental GC
    await ping();
    let r2 = Prim.rts_reclaimed();
    // ensure that b is retained
    assert(b.size() == length);

    Prim.debugPrint(
      "Ignore Diff: " #
      debug_show r0 # " " #
      debug_show r1 # " " #
      debug_show r2 # " "
    );

    // Checks that GC correctly discards or retains the arrays (16 MB).
    // Using --forced-gc and allowing young collection for generational GC.
    // Consider extra memory allocated by GC such as:
    // * Remembered set for the generational GC
    // * In-heap mark stack for the incremental GC
    assert (+r1-r0 >= 4 * length);
    assert (+r2-r1 < 4 * length);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"

// ic-ref-run is too slow on CI

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run