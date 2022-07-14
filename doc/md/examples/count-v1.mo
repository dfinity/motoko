actor Counter_v1 {

  stable var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };
}
