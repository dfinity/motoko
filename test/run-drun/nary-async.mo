/* test n-ary async/await */

/* n-ary args */
actor {
  public func f0_0() : async () {};
  {
    let t = "0_0";
    ignore async {
      await f0_0();
      debugPrint t;
    };
  };

  public func f1_0(x:Int) : async () {
    assert(x == 1);
  };
  {
    let t = "1_0";
    ignore async {
      await f1_0(1);
      debugPrint t;
    };
  };

  public func f2_0(x:Int,y:Bool) : async () {
    assert(x==1);
    assert(y==true);
  };
  {
    let t = "2_0";
    ignore async {
      await f2_0(1,true);
      debugPrint t;
    };
  };

  public func f3_0(x:Int,y:Bool,z:Text) : async () {
    assert(x == 1);
    assert(y == true);
    assert(z == "a");
  };
  {
  let t = "3_0";
    ignore async {
      await f3_0(1,true,"a");
      debugPrint t;
    };
  };

  /* n-ary returns */

  public func g0_0() : async () {};
  {
    let t = "0_0";
    ignore async {
      await g0_0();
      debugPrint t;
    };
  };

  public func g0_1() : async Int {
     1;
  };
  {
    let t = "0_1";
    ignore async {
      let x = await g0_1();
      assert(x == 1);
      debugPrint t;
      x;
    };
  };

  public func g0_2() : async (Int,Bool) {
     (1,true);
  };
  {
    let t = "0_2";
    ignore async {
      let (x,y) = await g0_2();
      assert(x==1);
      assert(y==true);
      debugPrint t;
      (x,y);
    };
  };


  public func g0_3() : async (Int,Bool,Text) {
     (1,true,"a");
  };
  {
    let t = "0_3";
    ignore async {
      let (x,y,z) = await g0_3();
      assert(x==1);
      assert(y==true);
      assert(z=="a");
      debugPrint t;
      (x,y,z);
    };
  };
}
