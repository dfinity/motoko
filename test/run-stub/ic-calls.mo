actor X {

  public func A() : () {
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

  public func test() = ignore async {
    A();
    let 1 = await B(1);
    let (1,true) = await C(1,true);
    debugPrint("test");
  };

};
X.test();

