let a = actor {
  public func hello() : async Text {
    "Hello ";
  };
  public func world() : async Text {
    "World!\n"
  };
  public func go() : async () {
    print((await hello()) # (await world()));
  };
};

let _ = a.go()
