let a = actor {
  var c = 1;
  public func inc() {
    c += 1;
    debug_print_Nat c; debug_print "\n";
  };
  public func printCounter () {
    debug_print_Nat c; debug_print "\n";
  }
};

a.inc();
a.inc();
a.inc();
a.printCounter();

func test() {
  var i : Int = 10;
  while (i  > 0) {
    a.inc();
    i -= 1;
  }
};

let _ = test();
