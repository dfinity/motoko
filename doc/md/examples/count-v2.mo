import Debug "mo:base/Debug";

persistent actor Counter_v2 {
  var state : Int = 0; // implicitly `stable`

  public func increment() : async () {
    state += 1;
    Debug.print(debug_show (state));
  };
};
