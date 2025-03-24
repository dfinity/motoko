actor {
   public shared func f2(_ : {#one; #three}, _ : { oldField : Nat; newField: Text }) : async { } {
      loop {};
   };
   stable let x2 = f2;
};
