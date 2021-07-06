import Prim "mo:⛔";

actor a {
   type List = ?([var Int], List);
   var list : List = null;

   public func grow(n : Nat) : async Nat {
     list := ?(Prim.Array_init(16 * 1024 * 256, 0),list);
     return  Prim.rts_memory_size();
   };

   public func go() : async () {
     var i = 0;
     var pre = Prim.rts_memory_size();
     Prim.debugPrint(debug_show((pre,Prim.natToNat32(pre))));
     while(i < 32) {
       assert(Prim.rts_memory_size() >=
         Prim.rts_heap_size());
       let size = await grow(i);
       Prim.debugPrint(debug_show(size, Prim.natToNat32(size)));
       assert (pre <= size);
       pre := size;
       i += 1;
     };
   }
};

await a.go(); //OR-CALL ingress go "DIDL\x00\x00"

// no point running these in the interpreter
//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run