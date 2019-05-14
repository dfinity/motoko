let a = actor {
  hello () : async Text {
    "Hello ";
  };
  world () : async Text {
    "World!\n"
  };
  go () : async () {
    print((await hello()) # (await world()));
  };
};

ignore(a.go())
