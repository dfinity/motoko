//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
 persistent class Outer<A, B>(outerA: A, outerB: B) {
    public func getA(): A {
      outerA;
    };

    public func getB() : B {
      outerB;
    };  

    public persistent class Test<X, Y>(initialX : X, initialY : Y) {
      var x = initialX;
      var y = initialY;

      public func getX() : X {
        x;
      };

      public func getY() : Y {
        y;
      };

      public func setX(newX : X) {
        x := newX;
      };

      public func other<Z>(z : Z) : Z {
        func test(): Z {
          z;
        };
        test();
      };
    };
  };

  let outer = Outer<Text, Nat>("", 1);
  let instance = outer.Test<Nat, Int>(0, +1);
  ignore instance.other(1);
};

//CALL upgrade ""
