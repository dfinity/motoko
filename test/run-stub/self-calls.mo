actor a {
  public func A() : async () {
    debugPrint("A called");
  };


  public func B(x : Int) : async Int {
    debugPrint("B called");
    x
  };

  public func C(x : Int, y: Bool) : async (Int,Bool) {
    debugPrint("C called");
    (x,y);
  };

  public func test() = ignore async {
    debugPrint("test1");
    let x = await a.A();
    debugPrint("test2");
    let y = await a.B(1);
    debugPrint("test3");
    let z = await a.C(2,true);
    debugPrint("test4");
    assert (y == 1);
    assert (z.0 == 2);
    assert (z.1);
  };
};
a.test();
