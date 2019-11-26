actor a {
  public shared func id(i : Nat) : async Nat { i };

  public shared func test() : async () {

    {
      let o = {var l = 0};
      o.l := 1;
      debugPrintNat(o.l);
      assert o.l == 1;
    };

    {
      let o = {var l = 0};
      o.l := await async 1;
      debugPrintNat(o.l);
      assert o.l == 1;
    };

    {
      let a = [var 0];
      a[await async 0] := 1;
      debugPrintNat(a[0]);
      assert a[0] == 1;
    };

    {
      let a = [var 0];
      a[await async 0] := await async 1;
      debugPrintNat(a[0]);
      assert a[0] == 1;
    };

    {
      let a = Array_init<Nat>(5, 0);
      for (i in a.keys()) {
        a[await id(i)] := await id(i);
        debugPrintNat(a[i]);
        assert a[i] == i;
      };
    };


  };

};

a.test();