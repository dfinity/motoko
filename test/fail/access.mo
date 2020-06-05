
let k = 1;
var j : Nat = 1;
let a = [var 1];
let o = { var k = 1 };
module Z = { public let x = 1 };

// actor class; restrict free vars
actor class C() : async actor {} {
  flexible let _ : Any = (k,j,a,o);
  flexible var l = 0;
  l := 1;
  actor class D() {
    flexible var d : Any = 0;
    d := l;
  };
};

let _ = { 
    actor N = { // nested actor, restrict free vars
      flexible let _ : Any = (j, a, o); // can't access j a
      flexible let _ : Any = (k, Z, Z.x); //ok
      flexible var z = 2;
      public func m () { 
        let _ = (z, N); // ok
        let _  = j; // can't access j
      };
    };
};


//main actor, unrestricted free vars
actor A = {
  flexible let _ = (k,j,a,Z, Z.x); // all ok
  flexible var z = 2;
  public func m () { let _ = (z, A, j); }; // all ok

}


