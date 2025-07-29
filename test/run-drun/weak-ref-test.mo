import Prim "mo:prim";

persistent actor {
  var arr = Prim.Array_init<Nat>(13, 0);
  let arr2 = [1, 2, 3];
  let aText = "Hello, world!";

  public func test() : async () {

    let wr1 = Prim.allocWeakRef(arr);
    let isLive1 = Prim.isLive(wr1);
    let deref1 = Prim.weakGet(wr1);

    Prim.debugPrint(debug_show wr1);
    Prim.debugPrint(debug_show isLive1);
    Prim.debugPrint(debug_show deref1);

    // Trigger GC by yielding to the scheduler.
    // The weak ref isLive should be false now and the deref should be null.
    var n = 10;
    while (n > 0) {
      n -= 1;
      // Allocate large array.
      arr := Prim.Array_init<Nat>(1_000, 1);
      await async {};
      let isLive1 = Prim.isLive(wr1);
      let deref1 = Prim.weakGet(wr1);
      Prim.debugPrint(debug_show isLive1);
      Prim.debugPrint(debug_show deref1);
    };

  };
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress test "DIDL\x00\x00"
