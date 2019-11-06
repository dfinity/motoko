func foo() { }; // <-- removing this definition avoids the compiler exception

actor class Test() = this { // <-- removing this stuff around the inner block avoids compiler exception
  public func go() {
    ignore(future
    {
      let x = 123;
      let x = 123;
    }
    )
  }
};
