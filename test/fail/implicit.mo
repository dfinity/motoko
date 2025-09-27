
type Order = {#less;#greater;#equal};

module M {
  public func c(n : Nat, m: Nat) : Order {
    if (n < m) #less
    else if (n == m) #greater
    else #equal;
  };
};

module N {
  public func c(n : Text, m: Text) : Order {
     #equal;
  };
};

func f1(n : Nat, m : Nat, c : (implicit : (Nat, Nat) -> Order)) {
  ignore c(n, m);
};

func f2<T>(n : T, m : T, c : (implicit : (T, T) -> Order)) {
  ignore c(n, m);
};

func f3(n : Nat, m : Nat, d : (implicit : Nat -> Order)) {
};


f1(0, 1, M.c); //accept
f2(0, 1, M.c); //accept

f1("0", "1", M.c); // reject
f2("0", "1", N.c); // accept

f1(0, 1); //accept
f2(0, 1); //accept

f2("0", "1"); // accept

f2(true, true); // reject

f3(1, 1); // reject

func f4(n : Nat, m : Nat, d : (implicit : (Nat, Nat) -> Order)) {
};


f4(1, 1); // reject

// retype f4 with as f4 with different implicit name
let f5 : (Nat, Nat, (c : (implicit : (Nat, Nat) -> Order))) -> () = f4;

f5(1, 1); // accept
