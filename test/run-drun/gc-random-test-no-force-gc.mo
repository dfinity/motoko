//MOC-NO-FORCE-GC
//MOC-FLAG --stable-regions
//MOC-FLAG --max-stable-pages=1000000

import GCRandomTest "gc-random-test/gc-random-test";

actor {
    let test = GCRandomTest.GCRandomTest();

    public shared func run() : async () {
        await test.run(120);
    };
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//CALL ingress run "DIDL\x00\x00"
