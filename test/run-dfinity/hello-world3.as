let a = actor {
  hello () {
    print("Hello ");
  };
  go () {
    hello();
    world();
  };
  world () {
    print("World!\n");
  };
};

a.go()
