import Prim "mo:⛔";
actor {

  flexible let array = [var 0, 0, 0];
  flexible let obj = { var field = "bad"; extra = 0 };

  // checks that decoding still works if the first
  // occurrence of the aliased object is ignored due to subtyping
  stable let a = (() : Any, array);
  stable let b = (() : Any, obj);

  public query func checkArray2() : async () {
    for (i in a.1.keys()) {
      assert(a.1[i] == i+1);
    };
  };

  public query func checkRecord2() : async () {
    Prim.debugPrint(b.1.field);
    assert(b.1.field != "bad");
    assert(b.1.field == "hello");
    assert(b.1.extra == 1);
  };
}
