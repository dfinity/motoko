let a = actor {
  public func hello() : future Text {
    "Hello ";
  };
  public func world() : future Text {
    "World!\n"
  };
  public func go() : future () {
    print((await hello()) # (await world()));
  };
};

let _ = a.go()
