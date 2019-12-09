actor {
  public query func absolute(x:Int) : async Nat {
    abs x
  };
  public query func absolutes(xs:[Int]) : async [Nat] {
    Array_tabulate<Nat>(xs.len(), func (i:Int) : Nat = abs(xs[i]))
  };
}

// with Nat
//CALL query absolute "DIDL\x00\x01\x7d\x2a"
// with Int
//CALL query absolute "DIDL\x00\x01\x7c\x56"

// with Nats
//CALL query absolutes "DIDL\x01\x6d\x7d\x01\x00\x02\x2a\x19"
// with Ints
//CALL query absolutes "DIDL\x01\x6d\x7c\x01\x00\x02\x56\x19"

//SKIP run
//SKIP run-ir
//SKIP run-low
