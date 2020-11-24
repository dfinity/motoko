ignore (switch 0 { case (x:Nat) x}) : Nat; // reject
ignore (switch 0 { case (x:Int) x}) : Nat; // reject
ignore (switch {x = 1} { case (x:{}) x}) : {}; // reject
