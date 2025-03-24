import Prim "mo:prim";

// Incompatible upgrade
actor {
    type RecursiveArray = ?[RecursiveArray];

    stable var simpleArray: [var Float] = [var];
    stable var nestedArray: [Nat] = [];
    stable var recursiveArray = ?[] : RecursiveArray;

    public func print() : async () {
        Prim.debugPrint(debug_show (simpleArray));
        Prim.debugPrint(debug_show (nestedArray));
        Prim.debugPrint(debug_show (recursiveArray));
    };
};
