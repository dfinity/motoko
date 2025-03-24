import Prim "mo:⛔";

// illegal textual principal (30 bytes decoded)
let t = "yvtf6-waaae-bagba-faydq-qcikb-mga2d-qpcai-reeyu-culbo-gazdi-nryhi";
// construct an actor from an illegal 30 byte principal
let bad = Prim.principalOfActor(actor (t)); // should trap
Prim.debugPrint(debug_show(bad));
