//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
    persistent class Test<X <: Nat>(x: X) {
        public func method() : X {
            x;
        };
    };

    func flexibleFunction() {};

    let test = Test<() -> ()>(flexibleFunction);
    let method = test.method;
    method();
};

//CALL upgrade ""
