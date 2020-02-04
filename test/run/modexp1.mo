
module X {
  public func f() { g() };
  func g() { f() };
};

let ok = X.f;
