persistent actor {
  persistent func stableActorFunction1() {};

  // drop stableActorFunction2()

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
  stableObject.set(stableActorFunction1);
  let stableFunction = stableActorFunction3;
};
