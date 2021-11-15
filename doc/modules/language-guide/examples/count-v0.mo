actor Counter_v0 {

  var value : Int = 0;

  public func inc() : async Int {
    value += 1;
    return value;
  };

}
