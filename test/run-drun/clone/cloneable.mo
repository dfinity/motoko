import Prim "mo:prim";
import Cycles = "../cycles/cycles";

// A simple Clonable class implementing a clone method without recursion.
// (init parameter, simple state, and a single other method for
//  illustration only)

actor class Cloneable(
  makeCloneable: shared (init : Nat) -> async Cloneable,
  init : Nat) {

  var state = init;
  Prim.debugPrint(debug_show(state));

  public func someMethod() : async () {
    state += 1;
    Prim.debugPrint(debug_show(state));
  };

  // our clone methods, indirecting through makeCloneable
  public func clone(init : Nat) : async Cloneable {
    Cycles.add(Cycles.balance() / 2);
    await makeCloneable(init : Nat);
  }
}
