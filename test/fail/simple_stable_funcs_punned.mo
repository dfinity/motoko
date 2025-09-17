type Order = {#less; #equal; #greater};

module Set {

  public type Cmp<T> = (T,T) -> Order;

  public type List<T> = ?(T, List<T>);
  public type Set<T, C <: Cmp<T>> = (C, ?(T, List<T>));

  public func empty<T, C <: Cmp<T>>(c: C) : Set<T, C> = (c, null);

  public func add<T, C <: Cmp<T>>(s : Set<T, C>, v : T) : Set<T, C> {
    let (cmp, l) = s;
    func add(l  : List<T>) : List<T> {
      switch l {
        case null { ?(v, null) };
        case (?(w, r)) {
          switch (cmp(v, w)) {
            case (#less) { ?(v, r) };
            case (#equal) { l };
            case (#greater) { ?(w, add(r)) };
          };
        };
      };
    };
    (cmp, add(l));
  };

  public func mem<T, C <: Cmp<T>>(s : Set<T, C>, v : T) : Bool {
    let (cmp, l) = s;
    func mem(l : List<T>) : Bool {
      switch l {
        case null { false };
        case (?(w, r)) {
          switch (cmp(v, w)) {
            case (#less) { false };
            case (#equal) { true };
            case (#greater) { mem(r) };
          };
        };
      };
    };
    mem(l);
  };
};

actor Ok {

  // stable func dec
  stable func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable let s1 : Set.Set<Int, cmp> = Set.empty<Int, cmp>(cmp);

  type T = cmp;

  stable let s2 : Set.Set<Int, T> = Set.empty<Int, T>(cmp);

};


/*
actor Ok1 {

  func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  transient let s1 : Set.Set<Int, (Int,Int) -> Order> = Set.empty<Int, (Int, Int) -> Order> (cmp);

};
*/
actor Bad1 {

  /* stable */ func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable let s1 = Set.empty<Int, (Int,Int) -> Order>(cmp); // reject, non-stable Cmp

};


actor Bad2 {

  /* stable */ func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable let s1 = Set.empty<Int, (Int,Int) -> Order>(cmp); // reject, non-stable cmp

};


actor Bad3 {

  stable func inc(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable func dec(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  stable let s1 = Set.empty<Int, inc>(inc);

  stable let s_ok = Set.add<Int, inc>(s1, 1); // accept, same comparison

  stable let s_fail = Set.add<Int, dec>(s1, 1); // reject, different comparison

};


