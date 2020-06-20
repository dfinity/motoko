import P "mo:prim";
actor a {
  public type Expression = {
    #const : Int;
    #add : (Expression, Expression);
    #mul : (Expression, Expression);
    #sub : (Expression, Expression);
  };

  func eval(exp : Expression) : Int {
    switch (exp) {
      case (#const(n)) n;
      case (#add(e1, e2)) eval(e1) + eval(e2);
      case (#mul(e1, e2)) eval(e1) * eval(e2);
      case (#sub(e1, e2)) eval(e1) - eval(e2);
    }
  };


  func evil(exp : Expression) : async Int {
    switch (exp) {
      case (#const(n)) n;
      case (#add(e1, e2)) (await evil(e1)) + (await evil(e2));
      case (#mul(e1, e2)) (await evil(e1)) * (await evil(e2));
      case (#sub(e1, e2)) (await evil(e1)) - (await evil(e2));
    }
  };

  func sum(n : Int) : Expression {
     if (n <= 0)
       #const 0
     else #add(#const n,sum (n-1));
  };

  public func evaluate() : async () {
    P.debugPrint (debug_show(eval(sum(32))));
  };

  public func eviluate() : async () {
    P.debugPrint (debug_show(await(evil(sum(32)))));
  };


};

ignore a.evaluate(); //OR-CALL ingress evaluate "DIDL\x00\x00"
ignore a.eviluate(); //OR-CALL ingress eviluate "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
