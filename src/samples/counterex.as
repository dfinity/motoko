actor class Counter(i : Int) {
  private var c = i;    

  dec() {
   show(c);
   c -= 1;
  };

  read() : async Int { c };
};

func show(c : Int) {};

func showAsync(t:Text, a : async Int) {};

let c = Counter(10);

func testDec() {
  var i : Int = 10;
  while (i > 0) {
    c.dec();
    i -= 1;
  }
};


let _ = testDec();

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