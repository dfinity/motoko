func wantSeeSwap(x : Bool) : Nat =
    if (not x) 42 else 25;

ignore(wantSeeSwap(true));

// FHECK-LABEL: (func $wantSeeSwap
// FHECK-NEXT: local.get $x
// FHECK-NEXT: if (result i32)
// FHECK-NEXT: i32.const 50
// FHECK-NEXT: else
// FHECK-NEXT: i32.const 84

