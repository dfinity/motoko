
module X = {
  func f() { g();};
  private func g() { f();};
  private let y : Nat = 2;
  private type U = Int;
  type T = U;
};

type Ok = X.T;
type Wrong = X.U;

