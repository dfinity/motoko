import Prim "mo:⛔";
actor {
  public func reject1() : async () {
     Prim.debugPrint "1";
     throw (Prim.error("Error"));
     Prim.debugPrint "wrong";
  };

  public func reject2() : async () {
     Prim.debugPrint "1";
     try { throw (Prim.error("Error")) }
     catch e {};
     Prim.debugPrint "ok";
  };

  public func reject3() : async () {
     Prim.debugPrint "1";
     try { throw (Prim.error("Error")) }
     catch e {
      Prim.debugPrint "ok";
      throw e;
     };
     Prim.debugPrint "wrong";
  };

}

//CALL ingress reject1 RElETAAA
//CALL ingress reject2 RElETAAA
//CALL ingress reject3 RElETAAA

//SKIP run
//SKIP run-ir
//SKIP run-low

