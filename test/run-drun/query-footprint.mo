actor footprint = {
    stable var s : Nat64 = 42;
    stable var hello : Text = "Hey You!";
    var lost : Text = "This will be lost";

    public query func go() : async Nat64 {
        s - 42
    };

    system func preupgrade() {
        hello #= " Hello World!"
    }
};

footprint.go(); //OR-CALL query go "DIDL\x00\x00"
//CALL ingress stable-variable-footprint "DIDL\x00\x00"
//CALL query stable-variable-footprint "DIDL\x00\x00"
//CALL query stable-variable-footprint "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
