func f():() -> Int {
  { func g() : Int = x; // reference x
    return (func() : Int{ g(); }); // early exit omits definition of x
    let x:Int = 666;
    func():Int{777;};
  };
};

debugPrint "1";
let h = f();

debugPrint "2";
let wrong = h();
