persistent actor {
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

  let stableObject = StableClass<persistent () -> ()>(stableActorFunction1);
  stableObject.set(stableActorFunction2);
  let stableFunction = stableActorFunction3;

  persistent func flexibleActorFunction1() {};

  persistent func flexibleActorFunction2() {};

  persistent func flexibleActorFunction3() {};

  persistent class FlexibleClass<X>(functionParameter : persistent () -> ()) {
    var x: ?X = null;

    public func set(newX: X) {
      x := ?newX;
    };

    public func flexibleMethod() {};

    public func stableMethod() {};
  };

  transient let flexibleObject = FlexibleClass<persistent () -> ()>(flexibleActorFunction1);
  flexibleObject.set(flexibleActorFunction2);
  transient let flexibleFunction = flexibleActorFunction3;
};
