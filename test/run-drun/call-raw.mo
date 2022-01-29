import P "mo:⛔";

actor self {

  public shared func test() : async () {};

  public shared func go() {
      let blob = await P.call_raw(P.principalOfActor(self),"test", "\44\49\44\4C\00\00");
      assert (blob == ("\44\49\44\4C\00\00": Blob));
  }

};

//SKIP run
//SKIP run-low
//SKIP run-ir
//CALL ingress go 0x4449444C0000
//CALL ingress go 0x4449444C0000
