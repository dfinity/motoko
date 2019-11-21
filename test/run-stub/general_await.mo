// TODO: Remove Await. prefix from calls once supported
// Remove bug workaround
// Increase N (once perf improves)
actor Await {

  let N = 3; // number of dynamic waits (low coz slow)

  public shared func Ack<@>() : async(){
    debugPrint "\nAck"
  };

  public shared func Request<@>(i : Int) : async Int {
    debugPrint("\nRequest(");debugPrintInt(i);debugPrint(")");
    return i;
  };

  // Static parallel waiting:

  public shared func PA<@>() : async () {
    let a1 = Await.Ack<@>();
    let a2 = Await.Ack<@>();
    await a1;
    await a2;
  };

  public shared func PR<@>() : async (Int,Int) {
    let a1 = Await.Request<@>(1);
    let a2 = Await.Request<@>(2);
    (await a1, await a2)
  };

  // Dynamic parallel waiting for acknowledgements

  public shared func DPA<@>() : async () {
    let as: [async()]  = Array_tabulate<async ()>(N, func (_) { Await.Ack<@>(); });
    for (a in as.vals()) {
      await a;
    };
  };

  // Dynamic parallel waiting (with results)

  public shared func DPR<@>() : async [Int] {
    func f (i:Nat) : async Int = Await.Request<@>(i);
    let as = Array_tabulate<async Int>(N, f);
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
      let a = Await.Ack<@>();
      await Await.RPA<@>(n-1); // recurse
      await a;
    };
  };

  // Recursive parallel waiting (with results)

  public type List<Int> = ?(Int,List<Int>);

  public shared func RPR<@>(n:Nat) : async List<Int> {
    if (n == 0) null
    else {
      let a = Await.Request<@>(n);
      let tl = await Await.RPR<@>(n-1); // recurse
      ?(await a,tl)
    }
  };


  public shared func Test() : async () {

      await Await.PA<@>();

      switch (await Await.PR<@>()) {
        case (1,2) ();
        case _ (assert false);
      };

      await Await.DPA<@>();

      let rs = await Await.DPR<@>();
      for (i in rs.keys()) {
          assert rs[i] == i;
      };

      await Await.RPA<@>(N);

      var l = await Await.RPR<@>(N);
      for (i in revrange(N, 1)) {
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

//CALL ingress Test 0x4449444C0000

