func f():() -> Int {
  { func g() : Int = x; // reference x
    return (func() : Int{ g(); }); // early exit omits definition of x
    let x:Int = 666;
    func():Int{777;};
  };
};

debug_print "1\n";
let h = f();

debug_print "2\n";
let wrong = h();
