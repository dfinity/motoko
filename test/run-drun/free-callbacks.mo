import Prim "mo:⛔";
actor a {
  public func ping() : async () {
  };

  public func go() : async () {
    await ping();
    let s0 = Prim.rts_heap_size();
    // Large enough size to use new allocation partition for the incremental GC.
    let length = 8 * 1024 * 1024;
    var a = Prim.Array_init<()>(length, ());
    // discard bigger array a
    a := Prim.Array_init<()>(0, ());
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
    // Checks that GC correctly discards or retains the arrays.
    // Using --forced-gc and allowing young collection for generational GC.
    // It allows for some wiggle room
    let reserve = 20_000;
    assert (+s1-s0 < reserve);
    assert (+s2-s0 > 4 * +length - reserve);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
