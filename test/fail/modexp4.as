
module X = {
  func f() { g();};
  private func g() { f();};
  private module Y = {
   type U = U -> U;
  };
  type T = Y.U; // bad public field because Y.U private and unavoidable
  func h():T { h() }; // ditto
};


