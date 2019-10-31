actor {
  public func reject1() : async () {
     print "1";
     throw (error("Error"));
     print "wrong";
  };

  public func reject2() : async () {
     print "1";
     try { throw (error("Error")) }
     catch e {};
     print "ok";
  };

  public func reject3() : async () {
     print "1";
     try { throw (error("Error")) }
     catch e {
      print "ok";
      throw e;
     };
     print "wrong";
  };

}

//CALL ingress reject1 0x4449444C0000
//CALL ingress reject2 0x4449444C0000
//CALL ingress reject3 0x4449444C0000
