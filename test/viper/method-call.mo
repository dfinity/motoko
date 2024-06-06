actor MethodCall {
  func ignoreBool(_b : Bool) : () {
  };

  // take an Int argument and return it unmodified
  func idInt(n : Int) : Int {
    return n;
  };

  // take a Bool argument and return it unmodified
  func idBool(b : Bool) : Bool {
    return b;
  };

  public func ifThenElse(b : Bool, tru : Int, fls : Int) : async Int {
    ignoreBool(b);        // standalone method call
    let c1 = idBool(b);   // method call in let-declaration
    var c2 = idBool(b);   // method call in var-declaration
    c2 := idBool(c2);     // method call in assignment
    if (c1 and c2) {
      return idInt(tru);  // method call in return statement
    };
    return idInt(fls);    // method call in return statement
  };

  var boolFld: Bool = false;

  public func testBoolFld() : async Bool {
    boolFld := not boolFld;       // field assignment
    boolFld := idBool(boolFld);   // method call in field assignment
    return boolFld;
  };

}
