
module X = {
  public func f() { g();};
  func g() { f();};
  module Y = {
    public type U = U -> U;
  };
  public type T = Y.U; // bad public field because Y.U private and unavoidable
  public func h():T { h() }; // ditto
};


