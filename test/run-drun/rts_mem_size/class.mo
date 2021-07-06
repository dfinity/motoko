import Prim "mo:⛔";

actor class Class (size : Nat) {
   type List = ?([var Int], List);
   var list : List = null;

   public func grow(n : Nat) : async Nat {
     list := ?(Prim.Array_init(size, 0),list);
     return  Prim.rts_memory_size();
   };

};

