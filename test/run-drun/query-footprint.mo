actor footprint = {
    public query func go() : async Nat64 {
        0
    }
};

footprint.go(); //OR-CALL query go "DIDL\x00\x00"
//CALL ingress stable-variable-footprint "DIDL\x00\x00"
//CALL query stable-variable-footprint "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
