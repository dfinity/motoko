//MOC-FLAG -no-check-ir
import Prim = "mo:prim";
actor A {

   private var o : ?(async Int) = null;

   public shared func spawn() : async Int {
     Prim.debugPrint("exiting spawn()");
     return 666; // reply to my caller, resuming waiters
   };

   public shared func writer() : async () {
     o := ?(spawn());
     loop await async {
       Prim.debugPrint("loop");
     };
   };

   public shared func reader() : async () {
     switch o {
       case null { await reader() };
       case (?f) {
         Prim.debugPrint("awaiting f");
         let _ = await f
       }; // add “reply to my caller” to waiters of r
     }
   };

   public func go() : async () {
     ignore async {
       await writer();
       Prim.debugPrint("WTF writer exited!")
     };
     await async {};
     await reader();
   }

}
//CALL ingress go "DIDL\x00\x00"
//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
