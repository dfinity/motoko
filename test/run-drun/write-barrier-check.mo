import Prim "mo:⛔";

actor a {
  public func go() : async () {
    let anchor = Prim.Array_init<?[var Nat]>(1, null);
    await async {}; // Trigger GC.
    // Allocate enough memory to regularly schedule GC run.
    anchor[0] := ?Prim.Array_init<Nat>(8 * 1024 * 1024, 0);
    await async {}; // Trigger GC.
  }
};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
