actor {
  public func reject1() : async () {
     debug_print "1";
     throw (error("Error"));
     debug_print "wrong";
  };

  public func reject2() : async () {
     debug_print "1";
     try { throw (error("Error")) }
     catch e {};
     debug_print "ok";
  };

  public func reject3() : async () {
     debug_print "1";
     try { throw (error("Error")) }
     catch e {
      debug_print "ok";
      throw e;
     };
     debug_print "wrong";
  };

}

//CALL ingress reject1 0x4449444C0000
//CALL ingress reject2 0x4449444C0000
//CALL ingress reject3 0x4449444C0000
