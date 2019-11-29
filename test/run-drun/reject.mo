actor {
  public func reject1() : async () {
     debugPrint "1";
     throw (error("Error"));
     debugPrint "wrong";
  };

  public func reject2() : async () {
     debugPrint "1";
     try { throw (error("Error")) }
     catch e {};
     debugPrint "ok";
  };

  public func reject3() : async () {
     debugPrint "1";
     try { throw (error("Error")) }
     catch e {
      debugPrint "ok";
      throw e;
     };
     debugPrint "wrong";
  };

}

//CALL ingress reject1 0x4449444C0000
//CALL ingress reject2 0x4449444C0000
//CALL ingress reject3 0x4449444C0000

//SKIP run
//SKIP run-ir
//SKIP run-low
