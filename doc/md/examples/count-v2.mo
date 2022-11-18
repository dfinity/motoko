actor Counter_v2 {

  stable var state : Int = 0;

  public func inc() : async Int {
    state += 1;
    return state;
  };

  public query func read() : async Int { return state; }
}
