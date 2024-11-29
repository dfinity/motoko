//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
actor {
    class Test<X>() {
        var y : ?X = null;
        public func method(x : X) {
            y := ?x;
        };
    };

    let flexibleFunction = func() {};

    stable let test = Test<() -> ()>();
    stable let method = test.method;
    method(flexibleFunction);
};

//CALL upgrade ""
