
module X = {
  func f() { g();};
  private func g() { f();};
  private type U = Int -> Int;
  type T = U; // ok export, because U, though private, is avoidable
  func h():T { h() }; // ditto
};

type Ok = X.T;
let ok = X.h;
