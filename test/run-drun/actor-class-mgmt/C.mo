import Prim "mo:⛔";
actor class C(n : Nat) = this {
   stable var upgrades = 0;

   system func preupgrade () {
     upgrades += 1;
   };

   public func observe () : async {args : Nat;  upgrades : Nat} {
     { args = n;  upgrades }
   };
}
