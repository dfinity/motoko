// actor references

actor {
  public func quux() : async Nat {
    let orange = actor "IC:C0FEFED00D41" : actor { fubar(n : Nat) : async Nat };
    let n = await orange.fubar(45)
  }
}
