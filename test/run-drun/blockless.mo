actor A {
    public func foo() : async Int { 42 };

    public func state(shortCircuit : Bool) : async () {
      var changed = false;
      try await async { 
        let a = async ();
        await a;
        changed := true;
        if shortCircuit
          await* a // fast await
        else
          await a; // proper await
        assert false;
      } catch _ {};
      assert (if shortCircuit not changed else changed);
    };

    public func go() : async () {
      ignore await* A.foo();
      await A.state false;
      await A.state true;
    }
};

A.go() //OR-CALL ingress go 0x4449444C0000
