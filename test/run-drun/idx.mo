import CO4 "ok/idx-class"

actor a {
    type O1 = { field : Int }; // "\ba\94\93\00"
    type O2 = { a: Int; field : Int }; // "a\00\00\00\ba\94\93\00"
    type O3 = { a: Int; field : Int; other : Int }; // "a\00\00\00\ba\94\93\00\d0fv6"
    type O4 = { a: Int; foo : Int; field : Int; other : Int }; // "a\00\00\00\06\c7M\00\ba\94\93\00\d0fv6"

    func go1(o : O1) : () = inner o;
    func go2(o : O2) : () = inner o;
    func go3(o : O3) : () = inner o;
    public shared func go4(o : O4) : () { assert o.a == 25; assert o.field == 42; assert o.other == 83; assert o.foo == 8; inner o };

    func inner(o : O1) { assert o.field == 42 };

    public func go() : async () {
        go1({ field = 42 }); // field: 9671866
        go2({ a = 25; field = 42 });
        go3({ a = 25; field = 42; other = 83 });
        go4({ a = 25; foo = 8; field = 42; other = 83 });
        let co4 = CO4.CO4();
        let a = co4.a;
        go4(co4)
    }

};

await a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
