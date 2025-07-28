import Prim "mo:prim";

persistent actor {
  let arr = Prim.Array_init<Nat>(1024, 0);
  let arr2 = [1, 2, 3];
  let aText = "Hello, world!";

  public func test() : async () {

    let wr1 = Prim.allocWeakRef(arr);
    let isLive1 = Prim.isLive(wr1);
    Prim.debugPrint(debug_show { isLive1 = isLive1 });

  };
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress test "DIDL\x00\x00"
