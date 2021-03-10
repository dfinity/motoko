
// These tests use `debug_show` on the generated typrep to exercise the type
// rep generation code. Only works for non-cyclic types of course.


// Need to unlock prim to access `@TypRep` and the `gen_typrep` prim
//MOC-ENV MOC_UNLOCK_PRIM=yesplease

import Prim "mo:prim";

Prim.debugPrint (debug_show ((prim "gen_typrep" : ?Bool -> @TypRep) null));
Prim.debugPrint (debug_show ((prim "gen_typrep" : ?(Bool, Int) -> @TypRep) null));

// This is infinite, cannot print that:
// type T = ??T;
// Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));


//SKIP run
//SKIP run-ir
// NB: Run-low should work!
