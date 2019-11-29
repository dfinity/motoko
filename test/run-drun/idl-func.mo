type Func = shared Int -> async Func;

actor {
  public query func fun() : async ?Func {
        null
  };
  public query func fun2(arg : ?Func) : async () {
  };
}


//CALL query fun 0x4449444C0000
//CALL query fun2 0x4449444c026e016a017c0000010000

//SKIP run
//SKIP run-ir
//SKIP run-low
