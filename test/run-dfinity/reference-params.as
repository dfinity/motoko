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

// test passing a funcref to an actor
let c = actor {
  say_hi(f : shared () -> ()) {
    f();
  };
};
c.say_hi(a.hello);

// test passing a own funcref to an actor
let d = actor {
  say_hi(f : shared () -> ()) {
    f();
  };
  hello() {
    print("Hello Universe!\n");
  };
  go() {
    say_hi(hello);
  }
};
d.go();

// test passing a self to an actor
let e = actor this {
  hello() {
    print("Hello Galaxy!\n");
  };
  send_to(f : shared (actor { hello : () -> () }) -> ()) {
    f(this);
  }
};
e.send_to(b.say_hi);
