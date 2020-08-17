import Prim "mo:prim";
actor {
  public func go() : async() {
    try {
      Prim.debugPrint("Installing actor...");
      let id = await Prim.installEmptyActor();
      Prim.debugPrint(debug_show (Prim.principalOfActor id));
    } catch e {
      Prim.debugPrint("Exception: " # debug_show (Prim.errorMessage(e)));
    }
  };
}
//CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

