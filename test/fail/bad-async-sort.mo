actor {


  func f() : async* Nat {
    await* g();
  };

  func g() : async Nat {
   await f();
  };

  func anon1() : async () {
    await async* {};
  };

  func anon2() : async () {
    await* async {};
  };

}