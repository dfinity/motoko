import Debug "mo:base/Debug";

<<<<<<< HEAD:doc/old-docs/examples/count-v0transient.mo
<<<<<<<< HEAD:doc/old-docs/examples/count-v0transient.mo
actor Counter_v0 {
  transient var state : Nat = 0;
========
persistent actor Counter_v2 {
  var state : Int = 0; // implicitly `stable`
>>>>>>>> 5f39141e6294cc73474dccfafe42ae38313f79af:doc/md/examples/count-v2.mo
=======
actor Counter_v0 {
  transient var state : Nat = 0;
>>>>>>> 5f39141e6294cc73474dccfafe42ae38313f79af:doc/md/examples/count-v0transient.mo

  public func increment() : async () {
    state += 1;
    Debug.print(debug_show (state));
  };
};
