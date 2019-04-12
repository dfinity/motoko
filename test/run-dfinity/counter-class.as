actor class Counter(i : Int) {
  private var c = i;

  dec() {
   showCounter(c);
   c -= 1;
  };

  read() : async Int { c };
};

func showCounter(c : Int) {};

let c = Counter(10);

func test() {
  var i : Int = 10;
  while (i  > 0) {
    c.dec();
	  i -= 1;
  }
};

let _ = test();
