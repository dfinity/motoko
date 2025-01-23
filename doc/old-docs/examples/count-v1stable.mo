import Debug "mo:base/Debug";

actor Counter_v1 {
  stable var state : Nat = 0;

  public func increment() : async () {
    state += 1;
    Debug.print(debug_show (state));
  };
};
