actor A {
    public func foo() : async Int { 42 };
    public func go() : async Int { await* A.foo() }
}

//CALL ingress go 0x4449444C0000
