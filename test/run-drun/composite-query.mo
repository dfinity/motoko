import Class "composite-query/Class";

import Prim "mo:prim";

// test composite queries (within canister.)
actor Composites {

   var o : ?Class.Class = null;

   public func init() : async () {
     o := ? (await Class.Class());
     Prim.debugPrint ("init");
   };

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

   public composite query func cq4() : async () {
      let ?c = o;
      await c.q(); // should succeed (non-recursive)
   };

   public composite query func cq5() : async () {
      let ?c = o;
      await c.cq(); // should succeed (non-recursive)
   };

}

//SKIP ic-ref-run
//CALL ingress init 0x4449444C0000
//CALL query q 0x4449444C0000
//CALL query cq1 0x4449444C0000
//CALL query cq2 0x4449444C0000
//CALL query cq3 0x4449444C0000
//CALL query cq4 0x4449444C0000
//CALL query cq5 0x4449444C0000
