// Upgrade should fail (version 1 to 4).
actor {
   // Incompatible function type change:
   // - Return type is a sub-type (adding an object field).
   public shared func f2(_ : {#one}, _ : { oldField : Nat; newField: Text }) : async { newField: Bool } {
      loop {};
   };
   stable let x2 = f2;
};
