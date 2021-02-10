import Prim "mo:prim";

actor class Clone(makeClone: shared (init : Nat) -> async Clone,
                  init : Nat) = self {

    var state = init;

    Prim.debugPrint(debug_show(state));

    public func someMethod() : async () {
      state += 1;
    };

    public func clone(init : Nat) : async Clone {
      await makeClone(init : Nat);
    }
}
