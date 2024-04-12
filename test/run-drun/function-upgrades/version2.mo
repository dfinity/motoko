// Upgrade should fail (version 1 to 2).
actor {
   // Incompatible function type change:
   // - First parameter is a super-type of version 1 (adding option tag `#three`).
   public shared func f2(_ : {#one; #three}, _ : { oldField : Nat; newField: Text }) : async { } {
      loop {};
   };
   stable let x2 = f2;
};
