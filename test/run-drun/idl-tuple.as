actor {
  public func len2(x:Text, y:Text) : async (Int,Int) {
    (x.len(), y.len())
  }
}

//CALL query len2 "DIDL\x00\x02\x71\x71\x02Hi\x05World"
