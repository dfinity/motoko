//MOC-FLAG -no-check-ir
import Prim = "mo:prim";
actor A {

   private var o : ?(async Int) = null;

   public shared func spawn() : async Int {
     Prim.debugPrint("exiting spawn()");
     return 666; // reply to my caller, resuming waiters
   };

   public shared func writer() : async Text {
     o := ?(spawn());
     loop await async {
//       Prim.debugPrint("loop");
     };
   };

   public shared func reader() : async Text {
     switch o {
       case null { await reader() };
       case (?f) {
         Prim.debugPrint("awaiting f");
         let _ = await f;
         return "reader's result"
       }; // add “reply to my caller” to waiters of f
     }
   };

   public func go() : async () {
     ignore async {
       let result = await writer();
       Prim.debugPrint("writers() result: " # result )
     };
     await async {};
     let reader_result = await reader();
   }

}
//CALL ingress go "DIDL\x00\x00"
//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
