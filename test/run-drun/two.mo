actor {

    public func one() : async () {}

} and
(actor {

    public func two() : async () {}

})
//OR-CALL ingress one "DIDL\x00\x00"
//OR-CALL ingress two "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
