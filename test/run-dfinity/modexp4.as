
module X = {
  func f() { g();};
  private func g() { f();};
  private module Y = {
   type U = U -> U;
  };
  private type T = Y.U; // ok private field eventhough Y.U private and unavoidable
  private func h():T { h() }; // ditto
};


