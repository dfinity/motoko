import Prim "mo:prim";
actor a {

  public func m() : async () {
    Prim.debugPrint(debug_show {
      cycles = Prim.cyclesAvailable();
      deadline = Prim.replyDeadline()
    });
  };

  func sync() : () {
      Prim.debugPrint("f()");
  };

  func fstar() : async* () {
    Prim.debugPrint("fstar()");
    await (with cycles=200; timeout = 400) m();
    ignore (with cycles=400; timeout = 800) m();
  };

  func f() : async () {
    Prim.debugPrint("f()");
    await (with cycles=600; timeout = 1200) m();
    ignore (with cycles=800; timeout = 1600) m();
  };

  public func go() : async () {
      await (with cycles=100; timeout = 200) m();
      sync(); // some effect
      ignore      sync(); // some effect
      let _ = fstar(); // no effect
      ignore fstar(); // no effect
      await* fstar(); // some effect

      let _ = f(); // some effect
      ignore f(); // some effect
      await f(); // some effect

  };

};
await a.go();//OR-CALL ingress go "DIDL\x00\x00"
