ignore (switch 0 { case (x:Nat) x}) : Nat; // accept
ignore (switch 0 { case (x:Int) x}) : Nat; // reject
ignore (switch {x = 1} { case (y:{}) y}) : {x : Nat}; // reject
