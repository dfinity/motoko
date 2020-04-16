import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed");
}

//CALL up _out/upgrade0.wasm
//CALL query check "DIDL\x00\x01\x7d\x01"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x02"
//CALL ingress inc "DIDL\x00\x00"
//CALL up _out/upgrade1.wasm
//CALL query check "DIDL\x00\x01\x7d\x03"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x04"
//CALL up _out/upgrade2.wasm
//CALL query check "DIDL\x00\x01\x7d\x04"
//CALL ingress inc "DIDL\x00\x00"
//CALL query check "DIDL\x00\x01\x7d\x05"

//SKIP run
//SKIP run-ir
//SKIP run-low
