// actor references


actor {
  public func quux() : async Nat {
    let orange = actor "IC:C0FEFED00D41" : actor { fubar(n : Nat) : async Nat };
    let n = await (orange.fubar(45))
  }
}

/*
+actor-literal.mo:6.51-6.63: type error, calling a shared function not yet supported
   (This is a limitation of the current version.)
*/