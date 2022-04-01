//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

actor {
func serNat(x: Nat) : Blob = (prim "serialize" : Nat -> Blob) x;
func deserNat(x: Blob) : Nat = (prim "deserialize" : Blob -> Nat) x;

var n = Prim.nat64ToNat(1<<32);
var c = n;
public func go() : async () {
 while (n > 0) {
   //Prim.debugPrint(debug_show(n));

   if (n != deserNat(serNat(n))) {
     Prim.debugPrint(debug_show {failure = n});
   };
   if (n > 7919) { n -= 7919; } else { return; };
   c -= 1;
   if (c % 1024 == 0) {
       await async (); // trigger gc
   }
 }
}
}
//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP comp-ref
//CALL ingress go "DIDL\x00\x00"
