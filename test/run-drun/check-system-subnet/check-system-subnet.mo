// A simple actor that checks if the subnet is an application subnet.
// This is done by running some instructions and burning some cycles
// and checking if the cycle balance is affected.

import Prim "mo:⛔";

persistent actor  {

    public func test() : async () {
        var j : Nat = 0;
        while (j < 100_000) {
            j += 1;
        };

        Prim.debugPrint("Cycle balance: " # debug_show(Prim.cyclesBalance()));
    }
};
