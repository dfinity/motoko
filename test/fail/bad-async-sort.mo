actor {
  func f() : async* Nat {
    await* g(); // accept
  };

  func g() : async Nat {
   await f(); // reject
  };

  func anon1() : async () {
    await async* {}; // reject
  };

  func anon2() : async () {
    await* async {}; // accept
  };

  func anon3() : async () {
    ignore ((async {}) : async* ()); // reject
  };

  func anon4() : async () {
    ignore ((async* {}) : async ()); // reject
  };

}
