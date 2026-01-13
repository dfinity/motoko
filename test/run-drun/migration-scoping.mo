import v "mo:prim";
// compilation only test (correct lexical scoping)
// migration expression can't access pattern variables
// migration expression doesn't capture class parameters
(with migration = func ({}) : {} { v.debugPrint("ok"); {} })
shared(v) actor class C(v : Bool) = {
  assert v
};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP drun-run
