//MOC-FLAG --stable-var-footprint-query=stable-variable-footprint

import { queryStableVarFootprint } "mo:⛔";

actor footprint = {
    stable var hello : Text = "Hey You!";

    public func self() : async Nat64 {
        await queryStableVarFootprint(footprint, ?"stable-variable-footprint")
    };
}

//CALL ingress self "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
