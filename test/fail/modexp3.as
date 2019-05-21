module X = {
  func f() { g();};
  private func g() { f();};
  private type U = U -> U;
  type T = U; // bad public field because U private and unavoidable
  func h():T { h() }; // ditto
};
