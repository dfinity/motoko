module X {
   func f() {
    type Callbacks = {
      var call : Nat -> ();
    };
    let callbacks : Callbacks = {
      var call = func(_) {};
    };
    callbacks.call(1);
  };
};
