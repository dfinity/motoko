actor {

  flexible let array = [var 1, 2, 3];
  flexible let obj = { var field = "hello" };

  stable let a = (array, array);
  stable let b = (obj, obj);

  public query func check() : async () {
    // check that mutable values are properly aliased

    for (i in a.0.keys()) {
      assert(a.0[i] == a.1[i]);
      a.0[i] += 1;
      assert(a.0[i] == a.1[i]);
    };

    assert(b.0.field == b.1.field);
    b.0.field #= "!";
    assert(b.0.field == b.1.field);

  };
}
//CALL query check "DIDL\x00\x01\x7d\x01"
//CALL upgrade
//CALL query check "DIDL\x00\x01\x7d\x03"

//SKIP run
//SKIP run-ir
//SKIP run-low
