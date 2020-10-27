actor Counter {

  var count : Nat = 0;

  // ...

  public query func peek() : async Nat {
    count
  };

}
