import Prim "mo:prim";

shared actor class (a : Text) {
   Prim.debugPrint(a);

   public func test() : async () {
     Prim.debugPrint(debug_show a # " tested");
   };
}
