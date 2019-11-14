actor X {
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

  public func test() : async () {
    debugPrint("test1");
    let x = await X.A();
    debugPrint("test2");
    let y = await X.B(1);
    debugPrint("test3");
    let z = await X.C(2,true);
    debugPrint("test4");
    assert (y == 1);
    assert (z.0 == 2);
    assert (z.1);
  };
}
//CALL ingress test 0x4449444C0000
