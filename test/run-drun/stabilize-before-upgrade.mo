import Prim "mo:prim";

actor {
    stable var unsigned : Nat = 12345678901234567890123456789012345678901234567890123456789012345678901234567890;
    stable var signed : Int = -12345678901234567890123456789012345678901234567890123456789012345678901234567890;

    public func print() : async () {
        Prim.debugPrint(debug_show (unsigned));
        Prim.debugPrint(debug_show (signed));
    };
};

//CALL ingress print "DIDL\x00\x00"
//CALL ingress __motoko_stabilize_before_upgrade "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress print "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
