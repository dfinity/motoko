// This test checks that the IDL encoder/decode implements
// the field escape logic
actor {
  type R = {
    _0_ : Int;
    _1_ : Nat;
    future_ : Text
  };

  public func out() : future R {
    { _0_ = 0xFFFF;
      _1_ = 0x1000;
      future_ = "XXX"
    }
  };

  public func foo1() : future {foo_ : ()} { { foo_ = () } };
  public func foo2() : future {foo : ()} { { foo = () } };

  public func input(r : R) : future () {
  };
}

//CALL ingress foo1 0x4449444C0000
//CALL ingress foo2 0x4449444C0000
//CALL ingress out 0x4449444C0000
//CALL ingress input 0x4449444c016c03007c017dbcfef7b102710100ffff03802003585858
