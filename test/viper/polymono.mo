actor PolyMono {
  func ignoreValue<T>(_a : T) : () {
  };

  // take a generic argument and return it unmodified
  func id<T>(a : T) : T {
    return a;
  };

  // take two generic arguments and return the first
  func firstValue<T, U>(a : T, _b : U) : T {
    return id(a);
  };

  // a function that calls itself
  func recur<T>(a : T) : T {
    return recur(a);   // a recursive call
  };

  // a pair of mutually recursive functions
  func mut_recur_a<T>(x : T) : T { return mut_recur_b(x); };
  func mut_recur_b<T>(x : T) : T { return mut_recur_a(x); };

  public func f() : async () {
    let _x1 = recur(true);     // recur$Bool
    let _x2 = mut_recur_a(5);  // mut_recur_a$Nat, mut_recur_b$Nat
  };

  public func ifThenElse(b : Bool, tru : Int, fls : Int) : async Int {
    ignoreValue(b);            // standalone method call           ignoreValue$Bool
    let c1 = id(b);            // method call in let-declaration   id$Bool
    var c2 = id(b);            // method call in var-declaration   id$Bool
    c2 := firstValue(c2, tru); // method call in assignment        firstValue$Bool$Int
    c2 := firstValue(c2, c2);  // method call in assignment        firstValue$Bool$Bool
    if (c1 and c2) {
      return id(tru);  // method call in return statement  id$Int
    };
    return id(fls);    // method call in return statement  id$Int
  };
}
