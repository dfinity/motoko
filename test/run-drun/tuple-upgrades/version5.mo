import Prim "mo:prim";

// Incompatible update
actor {
    type TreeType = ?(Nat, TreeType, Null);

    stable var pair = (1, 2);
    stable var largerTuple = (0, "Test", 1.23, { key = 5; value = '_' }, [-1, 2, -3]);
    stable var tree: TreeType = ?(2, ?(1, null: TreeType, null), null);

    public func print() : async () {
        Prim.debugPrint(debug_show (pair));
        Prim.debugPrint(debug_show (largerTuple));
        Prim.debugPrint(debug_show (tree));
    };
};
