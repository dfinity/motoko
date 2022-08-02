import Prim "mo:⛔";

actor {
   type List = ?([var Int], List);
   var list : List = null;

   public func grow(n : Nat) : async Nat {
     list := ?(Prim.Array_init(n * 1024 * 256, 0),list);
     return  Prim.rts_memory_size();
   };

   public func go() : async () {
     var i = 0;
     var pre = Prim.rts_memory_size();
     Prim.debugPrint(debug_show((pre,Prim.natToNat32(pre))));
     while(i < 16) {
       assert(Prim.rts_memory_size() >=
         Prim.rts_heap_size());
       let size = await grow(i);
       Prim.debugPrint(debug_show({
         memory_size = size;
         mutator_instructions = Prim.rts_mutator_instructions();
         collector_instructions = Prim.rts_collector_instructions()
       }));
       assert (pre <= size);
       pre := size;
       i += 1;
     };
   }
};

//CALL ingress go "DIDL\x00\x00"

// no point running these in the interpreter
//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run
