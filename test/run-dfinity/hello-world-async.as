let a = actor {
  world () {
    print("World!\n");
  };
  go () {
    world();
    print("Hello ");
  };
};

a.go()
