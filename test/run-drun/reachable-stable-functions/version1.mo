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
  stableObject.set(stableActorFunction2);
  stable let stableFunction = stableActorFunction3;

  func flexibleActorFunction1() {};

  func flexibleActorFunction2() {};

  func flexibleActorFunction3() {};

  class FlexibleClass<X>(functionParameter : stable () -> ()) {
    var x: ?X = null;

    public func set(newX: X) {
      x := ?newX;
    };

    public func flexibleMethod() {};

    public func stableMethod() {};
  };

  let flexibleObject = FlexibleClass<stable () -> ()>(flexibleActorFunction1);
  flexibleObject.set(flexibleActorFunction2);
  let flexibleFunction = flexibleActorFunction3;
};
