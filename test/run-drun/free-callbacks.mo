import Prim "mo:⛔";
actor a {
  public func ping() : async () {
  };

  public func go() : async () {
    let s0 = Prim.rts_heap_size();
    let a = Prim.Array_init<()>(2500, ());
    await ping();
    let s1 = Prim.rts_heap_size();
    await ping();
    let s2 = Prim.rts_heap_size();
    // last use of a
    ignore(a);
    await ping();
    // now a should be freed
    let s3 = Prim.rts_heap_size();

    Prim.debugPrint(
      "Ignore Diff: " #
      debug_show s0 # " " #
      debug_show s1 # " " #
      debug_show s2 # " " #
      debug_show s3 # " "
    );
    // This checks that the array (10_000 bytes) has been allocated, but then
    // freed. It allows for some wiggle room
    assert (+s1-s0 > 5_000);
    assert (+s2-s0 > 5_000);
    assert (+s3-s0 < 5_000);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"


//SKIP run
//SKIP run-low
//SKIP run-ir
