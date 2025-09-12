//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
    persistent class Test<X>() {
        var y : ?X = null;
        public func method(x : X) {
            y := ?x;
        };
    };

    func flexibleFunction() {};

    let test = Test<() -> ()>();
    let method = test.method;
    method(flexibleFunction);
};

//CALL upgrade ""
