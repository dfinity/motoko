actor {
   public shared func f2(_ : {#one}, _ : { newField: Text }) : async { } {
      loop {};
   };
   stable let x2 = f2;
};
