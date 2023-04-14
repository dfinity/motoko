import Prim "mo:prim";

actor a {

  func expect<T>(a : () -> async* T) : async* T {
    await* a();
  };


  func mapPar<A,B>(args : [A], f : A -> async* B) : async* [B] {
     let vs = Prim.Array_init<?B>(args.size(), null);
     for (i in args.keys()) {
        vs[i] := ?(await* f(args[i]));
     };
     Prim.Array_tabulate<B>(args.size(), func i { let ?v = vs[i]; v });
  };

  public func f(a : Nat) : async Text { debug_show(a) };

  public func go() : async () {
     let x = await* expect<Nat>(func () : async* Nat {await async 1});
     assert x == 1;

     let vs = await* mapPar([0,1,2], func (a : Nat) : async* Text { await f(a) });

     Prim.debugPrint(debug_show vs);
  }
};

//CALL ingress go "DIDL\x00\x00"
