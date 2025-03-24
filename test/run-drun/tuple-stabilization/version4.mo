import Prim "mo:prim";

// Incompatible upgrade
actor {
    stable var largerTuple = ("Test", 0, 1.23, { key = 5; value = '_' }, [-1, 2, -3]);

    public func print() : async () {
        Prim.debugPrint(debug_show (largerTuple));
    };
};
