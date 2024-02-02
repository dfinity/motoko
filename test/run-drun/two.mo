actor A {

    public func one() : async () {}

} and
(actor {

    public func two() : async () {}

})
A.one(); //OR-CALL ingress one "DIDL\x00\x00"
A.two(); //OR-CALL ingress two "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
