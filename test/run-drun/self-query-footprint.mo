//MOC-FLAG --stable-var-footprint-query=stable-variable-footprint

import Prim "mo:⛔";

actor footprint = {
    stable var hello : Text = "Hey You!";

    public func self() : async Nat64 {
        await ((Prim.queryStableVarFootprint()) ())
    }
}

//CALL ingress self "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
