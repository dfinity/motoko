
type Order = {#less;#greater;#equal};

module M {

  public func compare(n : Nat, m: Nat) : Order {
    if (n < m) #less
    else if (n == m) #greater
    else #equal;
  };

};

func f1(n : Nat, m : Nat, c : (implicit: (Nat, Nat) -> Order)) {
  ignore c(n, m);
};

func f2<T>(n : T, m : T, c : (implicit : (T, T) -> Order)) {
  ignore c(n, m);
};

f1(0, 1, M.compare);
f2(0, 1, M.compare);

f1(0, 1);
f2(0, 1);


//f2(true, true);
