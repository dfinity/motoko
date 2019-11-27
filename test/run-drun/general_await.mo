actor Await {

  public shared func Ack<@>() : async(){
    debugPrint "\nAck"
  };

  public shared func Request<@>(i : Int) : async Int {
    debugPrint("\nRequest(");debugPrintInt(i);debugPrint(")");
    return i;
  };

  // Static parallel waiting:

  public shared func PA<@>() : async () {
    let a1 = Ack<@>();
    let a2 = Ack<@>();
    await a1;
    await a2;
  };

  public shared func PR<@>() : async (Int,Int) {
    let a1 = Request<@>(1);
    let a2 = Request<@>(2);
    (await a1, await a2)
  };

  // Dynamic parallel waiting for acknowledgements

  public shared func DPA<@>() : async () {
    let as: [async()]  = Array_tabulate<async ()>(10, func (_) { Ack<@>(); });
    for (a in as.vals()) {
      await a;
    };
  };

  // Dynamic parallel waiting (with results)

  public shared func DPR<@>() : async [Int] {
    func f<>(i:Nat) : async Int = Request<@>(i);
    let as = Array_tabulate<async Int>(10, f);
    let res = Array_init<Int>(as.len(),-1);
    for (i in as.keys()) {
//      res[i] := (await as[i]); <-- compiler bug (generates incorrect code)
      let a = await as[i];
      res[i] := a;
    };
    Array_tabulate<Int>(as.len(),func i = res[i])
  };

  // Recursive parallel waiting

  public shared func RPA<@>(n:Nat) : async () {
    if (n == 0) ()
    else {
      let a = Ack<@>();
      await RPA<@>(n-1); // recurse
      await a;
    };
  };

  // Recursive parallel waiting (with results)

  public type List<Int> = ?(Int,List<Int>);

  public shared func RPR<@>(n:Nat) : async List<Int> {
    if (n == 0) null
    else {
      let a = Request<@>(n);
      let tl = await RPR<@>(n-1); // recurse
      ?(await a,tl)
    }
  };


  public shared func Test() : async () {

      await PA<@>();

      switch (await PR<@>()) {
        case (1,2) ();
        case _ (assert false);
      };

      await DPA<@>();

      let rs = await DPR<@>();
      for (i in rs.keys()) {
          assert rs[i] == i;
      };

      await RPA<@>(10);

      var l = await RPR<@>(10);
      for (i in revrange(10, 1)) {
        switch (l) {
          case (?(h, t)) {
            assert (h == i);
            l := t;
          };
          case null (assert false);
        }
      };
  }
};

Await.Test();

//SKIP run-drun
//SKIP comp
