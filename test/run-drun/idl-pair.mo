type Pair = (Int,Int);
actor {
  public func len2(x:Text, y:Text) : future Pair {
    (x.len(), y.len())
  }
}

//CALL ingress len2 "DIDL\x00\x02\x71\x71\x02Hi\x05World"
