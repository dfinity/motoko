import Debug "mo:base/Debug";

actor Counter_v2 {
  stable var state : Int = 0;

  public func increment() : async () {
    state += 1;
    Debug.print(debug_show (state));
  };
};
