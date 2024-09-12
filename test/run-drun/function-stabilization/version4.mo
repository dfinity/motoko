actor {
   public shared func f2(_ : {#one}, _ : { oldField : Nat; newField: Text }) : async { newField: Bool } {
      loop {};
   };
   stable let x2 = f2;
};
