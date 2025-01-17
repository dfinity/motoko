import Prim "mo:⛔";

actor class LowMemoryActor(callback : shared () -> async ()) {
  system func lowmemory() : async* () {
    Prim.debugPrint("Low memory!");
    await callback();
    Prim.debugPrint("Low memory callback done");
  };

  type Node = {
    array : [var Nat];
    next : ?Node;
  };

  var root : ?Node = null;

  public func allocateMemory() : async () {
    let array = Prim.Array_init<Nat>(8 * 1024 * 1024, 0); // 32 GB on 32-bit, 64 GB on 64-bit.
    let node : Node = { array; next = root };
    root := ?node;
  };

  public func memorySize() : async Nat {
    await async {}; // Allocate GC reserve (because of `--force-gc` flag during drun testing).
    Prim.rts_memory_size();
  };
};
