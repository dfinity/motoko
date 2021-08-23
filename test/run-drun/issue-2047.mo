actor a {
  let fooArr /* : [None] */  = [];
  ignore (fooArr : [Any]);
  public func run() {
    for (f in fooArr.vals()) {
      await f();
    };
  };
};
