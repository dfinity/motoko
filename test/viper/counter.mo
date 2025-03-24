// @verify

actor Counter {
  var count = 0 : Int;

  public func increment() : async () {
    count += 1;
  };

  public func getCount() : async Int {
    return count;
  };

  public func setCount(n : Int) : async () {
    count := n;
  };
};
