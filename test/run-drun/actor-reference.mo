// actor references

actor {
  public func quux() : async Nat {
    let orange = actor "bfozs-kwa73-7nadi" : actor { fubar(n : Nat) : async Nat };
    let n = await orange.fubar(45);
    let tangerine = actor ("bfozs-kw" # "a73-7nadi") : actor { fubar(n : Nat) : async Nat };
    let m = await tangerine.fubar(45)
  }
}
