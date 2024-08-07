import { setCandidLimits; getCandidLimits } = "mo:⛔";

actor {

  let limits = { instructions = { factor = 1 : Nat64 ; bias = 3 : Nat64 };
                 allocations = { factor = 2 : Nat64; bias = 4 : Nat64 } };

  setCandidLimits<system>(limits);
  assert getCandidLimits<system>() == limits;

}

//SKIP run-low
//SKIP run
//SKIP run-ir
//SKIP ic-ref-run
