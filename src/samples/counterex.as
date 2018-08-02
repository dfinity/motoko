/* a simple Counter actor */
actor class Counter(i : Int) {
  private var c = i;    

  /* decrement() message */
  dec() {
   show(c);
   c -= 1;
  };

  /* awaitable read() method */
  read() : async Int { c };
};

/* dummy functions to show intermediate value in trace */
func show(c : Int) {};

func showAsync(t:Text, a : async Int) {};

/* create an actor */
let c = Counter(10);

/* issue ten calls to dec() */
func testDec() {
  var i : Int = 10;
  while (i > 0) {
    c.dec();
    i -= 1;
  }
};


let _ = testDec();

/* issue ten calls to dec() & read() */
func testRead() : async () {
  var i : Int = 10;
  while (i > 0) {
    c.dec();
    let t = c.read();
    showAsync("before",t);
    show(await t);
    showAsync("after",t);
    i -= 1;
  }
};

let _ = testRead();