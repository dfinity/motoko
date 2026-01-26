//MOC-FLAG --enhanced-orthogonal-persistence --default-persistent-actors --enhanced-migration

import Prim "mo:prim";

(with multi_migration = (func(_ : {}) : { var field2 : Nat } { { var field2 = 121 } }))
actor {
    var field1 : Nat;

    public func check() : async () {
        Prim.debugPrint(debug_show field1);
    };
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP wasm-run

//CALL ingress check "DIDL\x00\x00"
