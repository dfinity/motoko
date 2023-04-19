import Prim "mo:⛔";
actor class C(n : Nat) = this {
   stable var upgrades = 0;

   system func preupgrade () {
     assert not Prim.isController(Prim.principalOfActor this);
     upgrades += 1;
   };

   public func observe () : async {args : Nat;  upgrades : Nat} {
     { args = n;  upgrades }
   };
}
