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
    let () = await X.A();
    debugPrint("test2");
    let 1 = await X.B(1);
    debugPrint("test3");
    let (1,true) = await X.C(1,true);
    debugPrint("test4");
  };
}
//CALL ingress test 0x4449444C0000
