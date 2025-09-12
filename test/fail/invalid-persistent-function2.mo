import Prim "mo:prim";

persistent actor {
  type Wrapper = {
    run : persistent() -> ();
  };
  type Factory = () -> Wrapper;

  func test() : Factory {
    persistent class () {
      public func run() {
        Prim.debugPrint("Test");
      };
    };
  };

  test().run();
};
