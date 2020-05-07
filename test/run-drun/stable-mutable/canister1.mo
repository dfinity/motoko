actor {

  flexible let array = [var 1, 2, 3];
  flexible let obj = { var field = "hello"; extra = 1 };

  stable let a = (array, array);
  stable let b = (obj : { var field : Text} , obj);

  public query func checkArray() : async () {
    // check that mutable values are properly aliased

    for (i in a.0.keys()) {
      assert(a.0[i] == a.1[i]);
      a.0[i] += 1;
      assert(a.0[i] == a.1[i]);
    };
  };

  public query func checkRecord() : async () {

    assert(b.0.field == "hello");
    assert(b.1.field == "hello");
    b.0.field #= "!";
    assert(b.0.field == b.1.field);

    // Check that the extra field is still there
    assert(b.1.extra == 1);

  };
}
