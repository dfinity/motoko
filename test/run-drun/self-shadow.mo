actor foo {
  public func foo() {};

  func go() {
    let bar = actor bar { public func bar() {} }
  };

}
