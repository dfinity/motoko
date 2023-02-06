actor Counter_v0 {

  var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };

}
