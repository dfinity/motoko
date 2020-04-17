actor {
   let o = object {flexible let x = 1}; //reject
   stable type T = Int;  // reject
   flexible type U = Int; // reject
   flexible ignore 666; // reject
   flexible (); // reject

   stable var x : Int = 0;
   stable var y : [var Int] = [var]; // reject
   stable let (ok1,ok2) = (1,2);
   stable let f = func(){}; // reject
   flexible var z : [var Int] = [var];
   //public stable shared func pub() {}; //what does this even mean, actually? reject?
}