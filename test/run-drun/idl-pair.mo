type Pair = (Int,Int);
actor {
  public func len2(x:Text, y:Text) : async Pair {
    (x.len(), y.len())
  }
}

//CALL ingress len2 "DIDL\x00\x02\x71\x71\x02Hi\x05World"

//SKIP run
//SKIP run-ir
//SKIP run-low
