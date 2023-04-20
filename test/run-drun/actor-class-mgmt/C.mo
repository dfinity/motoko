import Prim "mo:⛔";
actor class C(n : Nat, contr : Principal) = this {
   stable var upgrades = 0;
   assert Prim.isController contr;

   system func preupgrade () {
     assert not Prim.isController(Prim.principalOfActor this);
     upgrades += 1;
   };

   public func observe () : async {args : Nat;  upgrades : Nat} {
     assert Prim.isController contr;
     { args = n; upgrades }
   };
}
