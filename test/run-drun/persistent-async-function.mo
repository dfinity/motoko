import Prim "mo:prim";

persistent actor {
  public persistent func test(): async() {
    Prim.debugPrint("Test");
  };

  let x = test;

  public func run(): async() {
    await x();
  }
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//CALL ingress run 0x4449444C0000
