module Set = {
  public type Order = {#less; #equal; #greater};

  public type List<T> = ?(T, List<T>);

  public type Cmp<T> = (T, T) -> Order;

  public type Set<T, C <: Cmp<T>> = {
     cmp : C;
     var list : ?(T, List<T>)
  };

  public type Self<T, C <: Cmp<T>> = Set<T, C>;

  public func set<T, C <: Cmp<T>>(cmp : C) : Set<T, C> =
    { cmp; var list = null };

  public func add<T>(set : Set<T, Cmp<T>>, v : T) {
    func add(l  : List<T>) : List<T> {
      switch l {
	case null { ?(v, null) };
	case (?(w, r)) {
	  switch (set.cmp(v, w)) {
	    case (#less) { ?(v, l) };
	    case (#equal) {l };
	    case (#greater) { ?(w, add(r)) };
	  };
	};
      };
    };
    set.list := add(set.list);
  };

  public func mem<T>(set : Set<T, Cmp<T>>, v : T)
  : Bool {
    func mem(l : List<T>) : Bool {
      switch l {
	case null { false };
	case (?(w, r)) {
	  switch (set.cmp(v, w)) {
	    case (#less) { false };
	    case (#equal) { true };
	    case (#greater) { mem(r) };
	  };
	};
      };
    };
    mem(set.list);
  };
  // ...
};
