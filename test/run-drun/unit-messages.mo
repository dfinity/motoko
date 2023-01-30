import Prim "mo:⛔";
// test compilaton of messages involving non-syntactic unit types
actor a {
  type Unit = ();

  public func f() : async Unit {
    Prim.debugPrint("f");
  };

  public func g (x : Unit) : async Unit {
    Prim.debugPrint("g");
  };

  public func h() : async () {
    Prim.debugPrint("h");
  };

  public func i(x : Unit) : async () {
    Prim.debugPrint("i");
  };


  public func go() : async () {
    let () = await f();
    let () = await g();
    let () = await i();
    let () = await f();

    let () = (await f()) : Unit;
    let () = (await g()) : Unit;
    let () = (await i()) : Unit;
    let () = (await f()) : Unit;

    let () = await async (() : ());
    let () = await async (() : Unit);
    let () : Unit = await async (() : ());
    let () : Unit = await async (() : Unit);

};

};
await a.go(); //OR-CALL ingress go "DIDL\x00\x00"