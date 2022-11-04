// This is fine
do {
  func foo() : Int { bar () };
  func baz() : Int { x };
  func bar() : Int { baz() };
  let x = 1;
  assert(foo() == 1);
};

// This still works
do {
  func foo() : Int { bar () };
  let bar = do {
    func baz() : Int { x };
    func bar() : Int { baz() };
  };
  let x = 1;
  assert(foo() == 1);
};

// The second iteration of the use-before-define check even catches this one
do {
  func foo() : Int { bar () };
  let bar = do {
    let eager = 1;
    ignore(eager);
    func baz() : Int { x };
    func bar() : Int { baz() };
  };
  let x = 1;
  assert(foo() == 1);
}
