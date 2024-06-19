//MOC-FLAG --experimental-stable-memory 0
import P "mo:⛔";

import {stableMemoryGrow= _} "mo:⛔";
actor {

  let _ = P.stableMemorySize;
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

