import Float "mo:base/Float";

actor Counter_v4 {
  stable var state : Float = 0.0;

  public func increment() : async () {
    state += 0.5;
  };

  public func decrement() : async () {
    state -= 0.5;
  };

  public query func read() : async Float {
    return state;
  };
};
