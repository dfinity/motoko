import Prim "mo:prim";
shared actor class(trap : Bool) {

   if trap { // conditionally generate a trap on install
     assert false;
   };

   public func test() : async () {
     Prim.debugPrint(debug_show trap # " tested");
   };

}
