//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
actor {
    class Test<X <: Nat>(x: X) {
        public func method() : X {
            x;
        };
    };

    let flexibleFunction = func() {};

    stable let test = Test<() -> ()>(flexibleFunction);
    stable let method = test.method;
    method();
};

//CALL upgrade ""
