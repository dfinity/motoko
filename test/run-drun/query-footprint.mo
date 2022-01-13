//MOC-FLAG --stable-var-footprint-query=stable-variable-footprint

actor footprint = {
    stable var s : Nat64 = 42;
    stable var hello : Text = "Hey You!";
    var lost : Text = "This will be lost";
    let e0 = "seed";
    let e1 = (e0, e0);
    let e2 = (e1, e1);
    let e3 = (e2, e2);
    let e4 = (e3, e3);
    let e5 = (e4, e4);
    let e6 = (e5, e5);
    let e7 = (e6, e6);
    let e8 = (e7, e7);
    let e9 = (e8, e8);
    let e10 = (e9, e9);
    stable var expl = e10;

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
//CALL query __get_candid_interface_tmp_hack "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
