// This test checks that the IDL encoder/decode implements
// the field escape logic
actor {
  type R = {
    _0_ : Int;
    _1_ : Nat;
    async_ : Text
  };

  public func out() : async R {
    { _0_ = 0xFFFF;
      _1_ = 0x1000;
      async_ = "XXX"
    }
  };

  public func foo1() : async {foo_ : ()} { { foo_ = () } };
  public func foo2() : async {foo : ()} { { foo = () } };

  public func input(r : R) : async () {
  };
}

//CALL ingress foo1 0x4449444C0000
//CALL ingress foo2 0x4449444C0000
//CALL ingress out 0x4449444C0000
//CALL ingress input 0x4449444c016c03007c017dbcfef7b102710100ffff03802003585858
