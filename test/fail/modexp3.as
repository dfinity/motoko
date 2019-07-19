module X = {
  public func f() { g();};
  func g() { f();};
  type U = U -> U;
  public type T = U; // bad public field because U private and unavoidable
  public func h():T { h() }; // ditto
};
