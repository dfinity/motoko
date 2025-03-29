// Upgrade succeeds.
actor {
   // Identical function type.
   public shared query func f0() : async () { loop {} };
   stable let x0 = f0;

   // Identical function type.
   public shared composite query func f1(_ : Nat, _ : Bool) : async (Nat, Bool) {
      loop {};
   };
   stable let x1 = f1;

   // Compatible change:
   // - Both parameters are changed to stable sub-types (removing an variant tag, demoting a field).
   // - Return type is changed to a super-type (promoting object field).
   public shared func f2(_ : {#one}, _ : { oldField : Nat }) : async { oldField : Int } {
      loop {};
   };
   stable let x2 = f2;
};
