import Debug "mo:base/Debug";

<<<<<<< HEAD:doc/old-docs/examples/count-v1stable.mo
persistent actor Counter_v1 {
  var state : Nat = 0; // implicitly `stable`
=======
actor Counter_v1 {
  stable var state : Nat = 0;
>>>>>>> 5f39141e6294cc73474dccfafe42ae38313f79af:doc/md/examples/count-v1stable.mo

  public func increment() : async () {
    state += 1;
    Debug.print(debug_show (state));
  };
};
