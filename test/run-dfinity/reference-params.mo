let a = actor {
  public func hello() {
    print("Hello World!\n");
  };
};
a.hello();

// test passing an actor to an actor
let b = actor this {
  public func say_hi(a : actor { hello : () -> () } ) {
    a.hello();
  };
};
b.say_hi(a);

// test passing a funcref to an actor
let c = actor {
  public func say_hi(f : shared () -> ()) {
    f();
  };
};
c.say_hi(a.hello);

// test passing a own funcref to an actor
let d = actor {
  public func say_hi(f : shared () -> ()) {
    f();
  };
  public func hello() {
    print("Hello Universe!\n");
  };
  public func go() {
    say_hi(hello);
  }
};
d.go();

// test passing a self to an actor
let e = actor this {
  public func hello() {
    print("Hello Galaxy!\n");
  };
  public func send_to(f : shared (actor { hello : () -> () }) -> ()) {
    f(this);
  }
};
e.send_to(b.say_hi);
