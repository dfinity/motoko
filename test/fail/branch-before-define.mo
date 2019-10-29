func f():() -> Int {
  { label exit : (() -> Int) {
      func g() : Int = x; // reference x
      break exit (func() : Int{ g(); }); // early exit omits definition of x
      let x:Int = 666;
      func():Int{777;};
    }
  };
};

print "1\n";
let h = f();

print "2\n";
let wrong = h();
