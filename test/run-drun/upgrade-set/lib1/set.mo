import Prim "mo:prim";
module Set {

  public type Order = {#less; #equal; #greater};

  public type Cmp<T> = (T,T) -> Order;

  public type List<T> = ?(T, List<T>);
  public type Set<T, C <: Cmp<T>> = (C, ?(T, List<T>));

  public func empty<T, C <: Cmp<T>>(c: C) : Set<T, C> = (c, null);

  public func add<T, C <: Cmp<T>>(s : Set<T, C>, v : T) : Set<T, C> {
    let (cmp, l) = s;
    ignore cmp(v,v);
    func add(l  : List<T>) : List<T> {
      Prim.debugPrint "add";
      switch l {
        case null {
         Prim.debugPrint "null";
         ?(v, null) };
        case (?(w, r)) {
          switch (cmp(v, w)) {
            case (#less) { Prim.debugPrint "lt";?(v, l) };
            case (#equal) { Prim.debugPrint "eq"; l };
            case (#greater) { Prim.debugPrint "gt"; ?(w, add(r)) };
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


