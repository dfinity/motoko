module {
  public func curry<A, B, C>(f: ((A, B)) -> C) : (A, B) -> C {
    func (fst : A, snd: B) : C {
      f((fst, snd));
    }
  };

  public func uncurry<A, B, C>(f: (A, B) -> C) : ((A, B)) -> C {
    func (p : (A, B)) : C {
      f(p.0, p.1);
    }
  };
}
