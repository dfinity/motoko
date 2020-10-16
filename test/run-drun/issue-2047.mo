actor a {
let fooArr = [];
public func run() {
  for (f in fooArr.vals()) {
    await f();
  };
};
};