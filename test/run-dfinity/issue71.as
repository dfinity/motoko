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

let _ = a.go()