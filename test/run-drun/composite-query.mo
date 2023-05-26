actor Composites {

   public query func q() : async () {
   };

   public composite query func cq1() : async () {
   };

   public composite query func cq2() : async () {
      await q();
   };

   public composite query func cq3() : async () {
      await cq2();
   };

}