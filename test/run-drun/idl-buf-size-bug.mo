import Prim "mo:prim";
actor {
  public func match (b : Int) : async () {
    assert (switch b {
      case (4_619_372_036_854_775_805) true;
      case _ false
    })
  };

  public func do () : async () {
    await match (4_619_372_036_854_775_805);
  }
};

//CALL ingress do 0x4449444C0000
