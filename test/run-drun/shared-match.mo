/* this test show a discrepancy between argument pattern match failure in
shared methods in the interpreter (which traps, aborting the caller)
and compiled code (which continues with the caller)
This is expected since the interpreter does not yet turn message traps into rejects.
*/
import Prim "mo:prim";
actor a {
  public func match_true(true) : async () {
    Prim.debugPrint "match_true";
  };
  public func go() {
    Prim.debugPrint "go1";
    try { await match_true(true) }
    catch e {};
    try { await match_true(false) }
    catch e {};
    Prim.debugPrint "go2";
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"