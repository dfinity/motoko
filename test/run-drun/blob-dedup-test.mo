import Prim "mo:prim";

persistent actor {

  public func test(b : Blob) : async () {
    Prim.debugPrint(debug_show (b));
  };

  public func test2() : async () {
    let blob1 : Blob = "!caf!hello";
    let blob2 : Blob = "!caf!world";
    let blob3 : Blob = "acaf!hello";
    await test(blob1);
    await test(blob2);
    await test(blob3);
    await test(blob1);
    await test(blob2);
    await test(blob3);
    var counter = 20;
    while (counter > 0) {
      await test(blob1);
      await test(blob2);
      await test(blob3);
      counter -= 1;
    };
  };

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY

//CALL ingress test2 "DIDL\x00\x00"
