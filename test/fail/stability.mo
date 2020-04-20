actor {
   flexible let o = object {
      flexible let x = 1 //reject
   };
   stable type T = Int;  // reject
   flexible type U = Int; // reject
   flexible ignore 666; // reject
   flexible (); // reject

   stable var x : Int = 0; //accept
   stable var y : [var Int] = [var]; // reject
   stable let z = 1; // accept
   stable let z1 : Nat = 1; //accept
   stable let ((z2 : Nat)) = 1; //accept
   stable let (p,q) = (1,2); // reject (for now, maybe forever)
   stable let (r,s) = if true (1,2) else (2,1); // reject forever, I suspect, as RHS non-canonical
   stable let f = func(){}; // reject
   flexible var a : [var Int] = [var]; //accept
   //public stable shared func pub() {}; //what does this even mean, actually? reject?

   var w : Int = 0; // accept as flexible, but warn?
   let u : Int = 0; // ditto

   // reject hash collisions
   stable var foo = ();
   stable var nxnnbkddcv = (); // reject due to collision

   public func pub(){}; // accept as flexible (no warning)
   func priv(){}; // accept as flexible but warn
   private func priv1(){}; // accept as flexible but warn
   shared func priv2(){}; // accept as flexible but warn
   public shared func pub2(){}; // accept as flexible (no warning)
}
