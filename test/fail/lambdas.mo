// moc lambdas.mo --package base $MOTOKO_BASE
import Array "mo:base/Array";

func map<T>(ar : [T], f : T -> T) : [T] = ar;

func main1(szczebrzeszyn : [Nat]) : [Nat] {
  // map(szczebrzeszyn, func x = x + 1);
  map(szczebrzeszyn, func(x : Nat) = x + 1);
  // Array.map<Nat, Nat>(ar, func x = x + 1);
};

func main2(szczebrzeszyn : [Nat]) : [Nat] {
  Array.map(szczebrzeszyn, func(x : Nat) = x + 1);
};

func regressionReturnUnit() {
  loop { if true return };
};

func similarRegression<A <: ()>(a : A) {
  if true return;
  a : A;
};
