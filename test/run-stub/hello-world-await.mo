let a = actor {
  public func hello() : async Text {
    "Hello ";
  };
  public func world() : async Text {
    "World!"
  };
  public func go() = ignore async  {
    debugPrint((await hello()) # (await world()));
  };
};

a.go()
