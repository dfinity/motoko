actor {
  public query func flip(x:Bool) : async Bool {
    not x
  }
}

//CALL query flip 0x4449444C00017e00
//CALL query flip 0x4449444C00017e01
//CALL query flip 0x4449444C00017e02

//SKIP run
//SKIP run-ir
//SKIP run-low
