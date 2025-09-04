actor {
  persistent func stableActorFunction1() {};

  persistent func stableActorFunction2() {};

  persistent func stableActorFunction3() {};

  persistent class StableClass<X>(functionParameter : persistent () -> ()) {
    var x: ?X = null;

    public func set(newX: X) {
      x := ?newX;
    };

    public func stableMethod() {
      functionParameter();
    };
  };

  stable let stableObject = StableClass<persistent () -> ()>(stableActorFunction1);
  stableObject.set(stableActorFunction2); // Keep stable function reference
  stableObject.set(stableActorFunction1); // But then make stableActorFunction2 unreachable
  stable let stableFunction = stableActorFunction3;

  // Drop all `flexibleActorFunctionX` and `FlexibleClass`.
};
