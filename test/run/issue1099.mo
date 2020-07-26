let M = module { public func foo() = () };
module {
  public func bar() = M.foo();
}
