import P "mo:prim";
// test lexically nested do async expressions
actor a {

  public func bar() : async Int {
     666
  };

  private func foo(n : Nat) : async Int = do async {
     let _ = await do async { "hello" };
     if (n == 0) {
       await do async { await bar(); }
     }
     else await { do async { await foo(n-1); }; }
  };

  public func go() : async () {
     assert 666 == (await foo(100));
  };

};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"

