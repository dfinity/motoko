import Set "Set";
module StableSet {

  public type Order = {#less; #equal; #greater};

  public type Cmp<T> = (T, T) -> Order;

  public class StableSet<T, C <: Cmp<T>>(c : C) {
    public let cmp = c;
    public var set = Set.empty<T>();
  };

  // enable dot notation
  public type Self<T> = StableSet<T, Cmp<T>>;

  public func add<T>(s : StableSet<T, Cmp<T>>, v : T) {
    s.set := Set.add(s.set, s.cmp, v)
  };

  public func mem<T>(s : Self<T>, v : T) : Bool {
    Set.mem(s.set, s.cmp, v : T)
  };

  public func map<T, U, D <: Cmp<U>>(
    s : Self<T>,
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


