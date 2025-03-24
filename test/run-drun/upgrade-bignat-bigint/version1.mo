import Prim "mo:prim";

actor {
   stable var number : Int = -1;

   public func modify() : async () {
      number -= 2;
   };

   public func print() : async () {
      Prim.debugPrint(debug_show (number));
   };
};
