import Prim "mo:prim";

actor (with migration =
       func({unstable1 : () -> () }) :
         { unstable2 : () -> (); // not stable
           var three : Text; // wrong type, reject
           var versoin : (); // unrequired/mispelled, reject
         }
         { { var three = "";
             var unused = ();
             var versoin = ();
             unstable2 = func () {};
         }
       }) {

   stable var version = 0;

   stable var three : [var (Nat, Text)] = [var];

   public func check(): async() {
     Prim.debugPrint (debug_show {three});
   }

};
