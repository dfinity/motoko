type Func = shared Int -> future Func;

actor {
  public query func fun() : future ?Func {
        null
  };
  public query func fun2(arg : ?Func) : future () {
  };
}


//CALL query fun 0x4449444C0000
//CALL query fun2 0x4449444c026e016a017c0000010000
