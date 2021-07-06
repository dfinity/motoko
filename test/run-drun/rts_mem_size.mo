import Prim "mo:⛔";
import Lib "rts_mem_size/class";

actor a {
   type List = ?([var Int], List);
   var list : List = null;

   let size = 16 * 1024 * 1024 / 4;

   public func grow(n : Nat) : async Nat {
     list := ?(Prim.Array_init(size, 0),list); // add a ca. 16 MB array
     return  Prim.rts_memory_size();
   };

   public func go() : async () {
     var remote = await Lib.Class(size);
     var i = 0;
     var pre = Prim.rts_memory_size();
     Prim.debugPrint(debug_show(pre));
     while(i < 8) {
       assert(Prim.rts_memory_size() >=
         Prim.rts_heap_size());
       let size = await grow(i);
       Prim.debugPrint("local: " # debug_show(size));
       let remote_size = await remote.grow(i);
       Prim.debugPrint("remote: " # debug_show(remote_size));
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