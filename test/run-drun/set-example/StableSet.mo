import Set "Set";
module StableSet {

  // set as ordered lists
  public type Order = {#less; #equal; #greater};

  public type Cmp<T> = (T, T) -> Order;

  public type Self<T, C <: Cmp<T>> = {
    cmp : C;
    var set : Set.Set<T>
  };

  public type StableSet<T, C <: Cmp<T>> = Self<T, C>;

  public func StableSet<T, C <: Cmp<T>>(cmp : C) : Self<T, C> = {
    cmp;
    var set = Set.empty<T>();
  };

  public func add<T>(s : Self<T, Cmp<T>>, v : T) {
    s.set := Set.add(s.set, s.cmp, v)
  };

  public func mem<T>(s : Self<T, Cmp<T>>, v : T) : Bool {
    Set.mem(s.set, s.cmp, v : T)
  };

  public func map<T, U, D <: Cmp<U>>(
    s : Self<T, Cmp<T>>,
    cmp : D,
    f : T -> U) :
    StableSet<U, D> {
    var us = Set.empty<U>();
    func add(ts : Set.Set<T>) {
      switch ts {
        case null {};
	case (?(t,ts)) {
  	  us := Set.add<U>(us, cmp, f(t));
	  add(ts)
      }
     }
    };
    add(s.set);
    { cmp;
      var set = us }
  }
};


