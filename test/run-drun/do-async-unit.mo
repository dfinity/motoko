import P "mo:prim";

actor a {

  private func doUnit() : async () = do async {
    let t = await async (); // await at unit type
    return t;
  };

  private func doText() : async () = do async {
    let t = await async "text"; // await at different type
    return;
  };

  private func doReturn() : async () = do async {
    return;
  };

  private func doExit() : async () = do async {
  };

  private func doThrow() : async () = do async {
    throw P.error("oops");
  };

  public func go() : async () {
    await doUnit();
    await doReturn();
    await doExit();
    try {
      await doThrow();
      assert(false);
    } catch (e) { assert P.errorMessage(e) == "oops";};
  }

};


a.go(); //OR-CALL ingress go "DIDL\x00\x00"
