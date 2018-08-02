// A simple Counter actor.

actor class Counter(i : Int) {
  private var c = i;    

  // Decrement counter
  dec() {
   show("dec", c);
   c -= 1;
  };

  // Read counter, asynchronous
  read() : async Int { c };
};

// Dummy functions to show intermediate value in trace.
func show(note : Text, c : Int) {};
func showAsync(note : Text, a : async Int) {};

// Create an actor.
let c = Counter(10);

// Issue ten `dec` messages.
func testDec() {
  var i : Int = 10;
  while (i > 0) {
    c.dec();
    i -= 1;
  }
};

testDec();

// Issue ten `dec` & `read` messages.
func testRead() : async () {
  var i : Int = 10;
  while (i > 0) {
    c.dec();
    let t = c.read();
    showAsync("before", t);
    show("await", await t);
    showAsync("after", t);
    i -= 1;
  }
};

testRead();
