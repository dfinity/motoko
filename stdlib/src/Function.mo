/**
[#mod-Function]
= `Function` -- Functions on functions
*/

module {
  public func compose<A, B, C>(f : B -> C, g : A -> B) : A -> C {
    func (x : A) : C {
      f(g(x));
    };
  };

  public func const<A, B>(x : A) : (B) -> A {
    func (_ : B) : A {
      x;
    };
  };

  public func const2<A, B, C>(x : A) : (B, C) -> A {
    func (_ : B, _ : C) : A {
      x;
    };
  };

  public func lift<A, B, C>(f: (A, B) -> C) : ((A, B)) -> C {
    func (p : (A, B)) : C {
      f(p.0, p.1);
    }
  };

  public func lower<A, B, C>(f: ((A, B)) -> C) : (A, B) -> C {
    func (fst : A, snd: B) : C {
      f((fst, snd));
    }
  };
}
