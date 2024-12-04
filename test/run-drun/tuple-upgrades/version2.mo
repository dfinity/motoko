import Prim "mo:prim";

// Incompatible upgrade
actor {
    stable var unit = (1);
    
    public func print() : async () {
        Prim.debugPrint(debug_show (unit));
    };
};
