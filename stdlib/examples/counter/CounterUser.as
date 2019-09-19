// this example will not compile to Wasm; 
// it uses some language features that we do not yet support in the compilation story

// for now, run this example using the interpreter, as follows:
// 
// $ asc -r CounterUser.as
// 
// Will print `44`, which we explain below.


// We define an actor class for creating counter actors,
// `MakeCounter`; we use it below.

actor class MakeCounter (init:Nat) {
  var cell : Nat = init;
  public func inc() : async () {
    cell += 1;
  };
  public func read () : async Nat {
    cell
  };
};

// Note: Compared to the code in `Counter.as`, the version above is an
// `actor class`, not merely an `actor` (object) with a pre-determined
// initial state (of zero).  

// Instead, this `actor class` generalizes a class of similar objects,
// and its corresponding constructor function accepts a natural number
// argument `init` for the initial value of the counter.

// Below, we instantiate the actor class above using the initial
// counter value `42`:

actor CounterUser {
  public func doThings () : async () {
    let c = MakeCounter(42);
    await (c.inc ());
    await (c.inc ());
    let x = c.read ();
    printNat (await x);
  }
};

// Finally, we invoke the `CounterUser`, telling the AS interpreter to
// send it this `doThings` message, which gives it control within the
// interpreter, and performs the message interaction specified above
// in `doThings` between the `CounterUser` actor object and the
// counter object `c` that it instantiates.

CounterUser.doThings();
