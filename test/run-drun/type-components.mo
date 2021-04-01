actor {
  private class C(x:Int) {
    public type T = Int;
    public let f = x;
  };


  private class D(x:Int) {
    public type T = Bool;
    public let f = x;
  };

  let eqC = C(1) == C(1);
  let eqD = D(1) == D(1);

//  let tc = debug_show(C(1));
//  let td = debug_show(D(1));


};


