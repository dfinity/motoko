actor {
   public shared query func f0() : async () { loop {} };
   stable let x0 = f0;

   public shared composite query func f1(_ : Nat, _ : Bool) : async (Nat, Bool) {
      loop {};
   };
   stable let x1 = f1;

   public shared func f2(_ : {#one}, _ : { oldField : Nat; newField: Text }) : async { } {
      loop {};
   };
   stable let x2 = f2;
};
