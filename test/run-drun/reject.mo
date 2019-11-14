actor {
  public func reject1() : async () {
     Debug.print "1";
     throw (error("Error"));
     Debug.print "wrong";
  };

  public func reject2() : async () {
     Debug.print "1";
     try { throw (error("Error")) }
     catch e {};
     Debug.print "ok";
  };

  public func reject3() : async () {
     Debug.print "1";
     try { throw (error("Error")) }
     catch e {
      Debug.print "ok";
      throw e;
     };
     Debug.print "wrong";
  };

}

//CALL ingress reject1 0x4449444C0000
//CALL ingress reject2 0x4449444C0000
//CALL ingress reject3 0x4449444C0000
