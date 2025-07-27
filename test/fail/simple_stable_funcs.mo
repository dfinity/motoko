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
  type Cmp = stable cmp (Int, Int) -> Order; // singleton stable func type

  stable let s1 : Set.Set<Int, Cmp> = Set.empty<Int, Cmp>(cmp);

};


actor Ok1 {

  func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  type Cmp = (Int, Int) -> Order;

  transient let s1 : Set.Set<Int, Cmp> = Set.empty<Int, Cmp>(cmp);

};

actor Bad1 {

  /* stable */ func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };
  type Cmp = (Int, Int) -> Order;

  stable let s1 = Set.empty<Int, Cmp>(cmp); // reject, non-stable Cmp

};


actor Bad2 {

  /* stable */ func cmp(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };
  type Cmp = stable cmp (Int, Int) -> Order;

  stable let s1 = Set.empty<Int, Cmp>(cmp); // reject, non-stable cmp

};


actor Bad3 {

  stable func inc(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };
  type Inc = stable cmp (Int, Int) -> Order;

  stable func dec(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };
  type Dec = stable dec (Int, Int) -> Order;

  stable let s1 = Set.empty<Int, Inc>();

  stable let s_ok = Set.add<Int, Inc>(s1, 1); // accept, same comparison

  stable let s_fail = Set.add<Int, Dec>(s1, 1); // reject, different comparison

};
