import P "mo:prim";
// test n-ary (n=2) async compilation
actor a {

  private func doUnit() : async ((),()) = do async {
    let t = await async (); // await at unit type
    return (t,t);
  };

  private func doText() : async ((),()) = do async {
    let t = await async "text"; // await at different type
    return ((),());
  };

  private func doReturn() : async ((),()) = do async {
    return ((),());
  };

  private func doExit() : async ((),()) = do async {
    ((),())
  };

  private func doThrow() : async ((),()) = do async {
    throw P.error("oops");
  };

  public func go() : async () {
    let ((),()) = await doUnit();
    let ((),()) = await doReturn();
    let ((),()) = await doExit();
    try {
      let ((),()) = await doThrow();
      assert(false);
    } catch (e) { assert P.errorMessage(e) == "oops";};
  }

};


a.go(); //OR-CALL ingress go "DIDL\x00\x00"
