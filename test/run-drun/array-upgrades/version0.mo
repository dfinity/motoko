import Prim "mo:prim";

// Compatible upgrade
actor {
    type RecursiveArray = ?[RecursiveArray];

    stable var simpleArray = [var 1.0, 2.0, 3.0, 4.0];
    stable var nestedArray = [[1, 2, 3], [4, 5, 6]];
    stable var recursiveArray = ?[null, ?[], ?[?[null, null], null]] : RecursiveArray;

    public func print() : async () {
        Prim.debugPrint(debug_show (simpleArray));
        Prim.debugPrint(debug_show (nestedArray));
        Prim.debugPrint(debug_show (recursiveArray));
    };
};
