import Debug "mo:base/Debug";
import Float "mo:base/Float";

actor Counter_v6 {
  stable var newState : Float = 0.0;

  public func increment() : async () {
    newState += 0.5;
  };

  public func decrement() : async () {
    newState -= 0.5;
  };

  public query func read() : async Int {
    Debug.trap("No longer supported: Use `readFloat`");
  };

  public query func readFloat() : async Float {
    return newState;
  };
};
