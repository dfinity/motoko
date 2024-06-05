//MOC-FLAG --stable-regions

import GCRandomTest "gc-random-test/gc-random-test";

actor {
    let test = GCRandomTest.GCRandomTest();
    
    public shared func run() : async () {
        await test.run(25);
    };
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//CALL ingress run "DIDL\x00\x00"
