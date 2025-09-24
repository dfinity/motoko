module Set = {
  public type Order = {#less; #equal; #greater};

  public type List<T> = ?(T, List<T>);

  public type Set<T> = ?(T, List<T>);

  public func empty<T>() : Set<T> = null;

  public func add<T>(s : Set<T>, cmp : (T,T) -> Order, v : T)
  : Set<T> {
    func add(l  : List<T>) : List<T> {
      switch l {
	case null { ?(v, null) };
	case (?(w, r)) {
	  switch (cmp(v, w)) {
	    case (#less) { ?(v, l) };
	    case (#equal) {l };
	    case (#greater) { ?(w, add(r)) };
	  };
	};
      };
    };
    add(s);
  };

  public func mem<T>(s : Set<T>, cmp : (T, T) -> Order,  v : T)
  : Bool {
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
    mem(s);
  };
  // ...
};
