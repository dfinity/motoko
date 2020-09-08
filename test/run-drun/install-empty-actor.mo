import Prim "mo:prim";
actor {

  let wasm_mod : Blob = "\00\61\73\6D\01\00\00\00";
  let empty_arg : Blob = "";

  public func go() : async() {
    try {
      Prim.debugPrint("Installing actor:");
      let principal = await Prim.createActor(wasm_mod, empty_arg);
      let id = debug_show principal;
      Prim.debugPrint(id);
      let a = actor (id) : actor { };
    } catch e {
      Prim.debugPrint("Exception: " # debug_show (Prim.errorMessage(e)));
    }
  };
}
//CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

