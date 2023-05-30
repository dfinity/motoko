// test composite queries (within canister.)
actor Composites {

   public query func q() : async () {
   };

   public composite query func cq1() : async () {
   };

   public composite query func cq2() : async () {
      await q(); // dynamically fails due to unsupported recursion
   };

   public composite query func cq3() : async () {
      await cq2(); // dynamically fails due to unsupported recursion
   };

}

//SKIP ic-ref-run
//CALL query q 0x4449444C0000
//CALL query cq1 0x4449444C0000
//CALL query cq2 0x4449444C0000
//CALL query cq3 0x4449444C0000