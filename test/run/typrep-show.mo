
// These tests use `debug_show` on the generated typrep to exercise the type
// rep generation code. Only works for non-cyclic types of course.


// Need to unlock prim to access `@TypRep` and the `gen_typrep` prim
//MOC-ENV MOC_UNLOCK_PRIM=yesplease

import Prim "mo:prim";

do {
  type T = Bool;
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = ();
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = ([Bool],?Int);
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = {first_name: Text; last_name: Text; points : Int};
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = {#monday; #tuesday; #gotboredday};
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = {#monday; #tuesday; #gotboredday};
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
do {
  type T = shared Text -> async Text;
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};

// This is infinite, cannot print that:
/*
do {
  type T = ?T
  Prim.debugPrint (debug_show ((prim "gen_typrep" : ?T -> @TypRep) null));
};
*/


//SKIP run
//SKIP run-ir
// NB: Run-low should work!
