//MOC-FLAG --stable-var-footprint-query=stable-variable-footprint

actor footprint = {
    let e0 = "seedseedseedseedseedseedseedseedseedseedseedseedseed";
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
    let e11 = (e10, e10);
    let e12 = (e11, e11);
    let e13 = (e12, e12);
    /*let e14 = (e13, e13);*/
    //let e15 = (e14, e14);
    //let e16 = (e15, e15);
    //let e17 = (e16, e16);
    //let e18 = (e17, e17);
    stable var expl = e13;

    public query func go() : async Nat64 {
        42
    };
};

//CALL query __get_candid_interface_tmp_hack "DIDL\x00\x00"
//CALL query stable-variable-footprint "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
