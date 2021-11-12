actor Counter {

  stable var value : Int = 0;

  public func inc() : async Int {
    value += 1;
    return value;
  };

  public func reset() : async () {
    value := 0;
  }
}
