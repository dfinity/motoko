import Prim "mo:prim";

actor class TestCanister() {
   stable var array : [var Nat] = Prim.Array_init<Nat>(16 * 1024 * 1024, 0xfff_ffff);

   Prim.debugPrint("array1 initialized, length: " # debug_show (array.size()) # ", memory: " # debug_show (Prim.rts_memory_size()));

   stable var version = 0;
   version += 1;

   Prim.debugPrint("version: " # debug_show(version));
};
