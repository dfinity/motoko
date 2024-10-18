actor class C(
   other : actor {
      __motoko_inspect_data : () -> async Blob;
   }
) {
   public shared func callback() : async Blob {
      await other.__motoko_inspect_data();
   };
};
