
module X = {
  func f() { g();};
  private func g() { f();};
};

let ok = X.f;
let wrong = X.g;
