// fails to send due to recursive implementation of Candid serialization
import Prim "mo:⛔";
actor a {

   type N = ?N;

   func toN(n : Int, k : N) : N {
       if (n == 0) k
       else toN(n - 1, ?k );
   };

   public func go () : async () {
      let n = toN(10000, null);
      await m(n);
   };

   public func m(n : N) : async () {
      Prim.debugPrint("done");
   };
};


//CALL ingress go 0x4449444C0000
//SKIP run
//SKIP run-ir
//SKIP run-low

