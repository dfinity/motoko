// test type components in object and actors (not just modules)

object X = {
  func f() { g();};
  private func g() { f();};
  object Y = {
   type U = Int;
  };
  type T = Y.U; // ok type field since Y.U private but avoidable
  private func h():T { h() }; // ditto
};

type XT = X.T;
type XYU = X.Y.U;


actor A = {
  func f() { g();};
  private func g() { f();};
  private object Y = {
   type U = Int;
  };
  type T = Y.U; // ok type field since Y.U private but avoidable
  private func h():T { h() }; // ditto
};

type AT = A.T;

