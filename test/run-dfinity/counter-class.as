actor class Counter(i : Int) {
  private var j = i;

  dec() {
   showCounter(j);
   j -= 1;
  };

  read() : async Int { j };
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

test();
