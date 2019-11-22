actor X {

  public func A() : async () {
    debugPrint("A");
  };


  public func B(x : Int) : async Int {
    debugPrint("B");
   x
  };

  public func C(x : Int, y: Bool) : async (Int,Bool) {
   debugPrint("C");
   (x,y);
  };

  public func test() : async () {
    let () = await A();
    let 1 = await B(1);
    let (1,true) = await C(1,true);
    debugPrint("test");
  };

}
//CALL ingress test 0x4449444C0000

