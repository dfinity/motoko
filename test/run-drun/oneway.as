actor {
  // test that oneways can locally try/throw
  public func oneway() : () {
    print "1";
    try {
      throw (error("Error"));
      print "unreachable";
    }
    catch e { print "2"};
  };

  // test that throws from oneways are silently discarded (because replies are eager)
  public func discard() : () {
    print "3";
    throw (error("ignored"));
    print "unreachable";
  };

}
//CALL ingress oneway 0x4449444C0000
//CALL ingress discard 0x4449444C0000
