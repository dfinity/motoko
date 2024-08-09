import { setCandidLimits; getCandidLimits } = "mo:⛔";

actor {

  let limits = { numerator = 1 : Nat64;
                 denominator = 2 : Nat64;
                 bias = 3 : Nat64 };

  setCandidLimits<system>(limits);
  assert getCandidLimits<system>() == limits;

}

//SKIP run-low
//SKIP run
//SKIP run-ir
//SKIP ic-ref-run
