import Prim "mo:⛔";
actor class C(n : Nat, contr : ?Principal) = this {
   stable var upgrades = 0;
   switch contr { case (?contr) assert Prim.isController contr; case _ () };

   system func preupgrade () {
     assert not Prim.isController(Prim.principalOfActor this);
     upgrades += 1;
   };

   public func observe () : async {args : Nat;  upgrades : Nat} {
     switch contr { case (?contr) assert Prim.isController contr; case _ () };
     { args = n; upgrades }
   };
}
