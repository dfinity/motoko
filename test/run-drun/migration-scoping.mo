import Prim "mo:prim";
// compilation only test (correct lexical scoping)
// migration expression can access pattern variables
// migration expression doesn't capture class parameters
shared(v) actor [func ({}) : {} { Prim.debugPrint (debug_show v); {} }] class
  C(v : Bool) = {
  assert v
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run
//SKIP drun-run
