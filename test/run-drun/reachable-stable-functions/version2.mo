actor {
  func stableActorFunction1() {};

  func stableActorFunction2() {};

  func stableActorFunction3() {};

  class StableClass<X>(functionParameter : stable () -> ()) {
    var x: ?X = null;

    public func set(newX: X) {
      x := ?newX;
    };

    public func stableMethod() {
      functionParameter();
    };
  };

  stable let stableObject = StableClass<stable () -> ()>(stableActorFunction1);
  stableObject.set(stableActorFunction2); // Keep stable function reference
  stableObject.set(stableActorFunction1); // But then make stableActorFunction2 unreachable
  stable let stableFunction = stableActorFunction3;

  // Drop all `flexibleActorFunctionX` and `FlexibleClass`.
};
