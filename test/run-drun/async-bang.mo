import Prim "mo:prim";

actor a {

  public func go() : async () {
    func print(o : ? Nat) { Prim.debugPrint(debug_show(o));};

    let o1 = ? {
        let oi = ?1;
        let oj = ?2;
        await async {};
        oi! + oj!;
    };
    print(o1);
    assert (o1 == ? 3);


    let o2 = ? {
        let oi = ?1;
        let oj : ?Nat = null;
        await async {};
        oi! + oj!;
    };
    print(o2);
    assert (o2 == null);


    let o3 = ? {
       var sum = 0;
       await async {};
       for(o in [?1, ?2, ?3].vals()) {
         sum += o!
       };
       sum
    };
    print(o3);
    assert (o3 == ? 6);

    let o4 = ? {
       var sum = 0;
       await async {};
       for(o in [?1, ?2, null].vals()) {
         sum += o!
       };
       sum
    };
    print o4;
    assert (o4 == null);

    /* nesting */

    let o5 = ? {
       let o = ??0;
       await async {};
       o!!
    };
    print o5;
    assert (o5 == ?0);

    let o6 : ? Nat = ? {
       let o = ?null;
       await async {};
       o!!
    };
    print o6;
    assert (o6 == null);

    let o7 = ? {
       let o = (null : ? None);
       await async {};
       o!!
    };
    print o7;
    assert (o7 == null);

  }

};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"
