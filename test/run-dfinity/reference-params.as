let a = actor {
  hello() {
    print("Hello World!\n");
  };
};
a.hello();

// test passing an actor to an actor
let b = actor this {
  say_hi(a : actor { hello : () -> () } ) {
    a.hello();
  };
};
b.say_hi(a);

// test passing an actor to an actor
let c = actor {
  say_hi(f : () -> ()) {
    f();
  };
};
c.say_hi(a.hello);
