// moc lambdas.mo --package base $MOTOKO_BASE
import Array "mo:base/Array";
import Iter "mo:base/Iter";

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

func regressionRecursion(n : Nat) {
  if (n == 0) return;
  regressionRecursion(n - 1);
};

func m1() {
  let array1 = [1, 2, 3];
  // let array2 = Array.map(array1, func x = x * 2)
  //                                  ^
  // cannot infer type of variableMotoko(M0103)
};

func m2() {
  let array1 = [1, 2, 3];
  let array2 = Array.map(array1, func(x : Nat) = x * 2)
  //           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // cannot implicitly instantiate function of type
  //   <T, R>(array : [T], f : T -> R) -> [R]
  // to argument of type
  //   ([Nat], (x : Nat) -> ())
  // because no instantiation of T, R makes
  //   ([Nat], (x : Nat) -> ())  <:  (array : [T], f : T -> R)Motoko(M0098)
};

func m3() {
  let array1 = [1, 2, 3];
  let array2 = Array.map(array1, func(x : Nat) : Nat = x * 2);
};

func m3_2() {
  let array1 = [1, 2, 3];
  let array2 = Array.map<Nat, Nat>(array1, func x = x * 2);
};

func n1() {
  let array1 = [1, 2, 3];
  // let array2 = array1 |> Iter.fromArray(_) |> Iter.map(_, func x = x + 1) |> Iter.toArray(_)
  // cannot infer type of variableMotoko(M0103)
};

func n2() {
  let array1 = [1, 2, 3];
  let array2 = array1 |> Iter.fromArray(_) |> Iter.map(_, func(x : Nat) = x + 1) |> Iter.toArray(_)
  // cannot infer type of variableMotoko(M0103)
};

// module AsyncStuff {
//   func call<T>(f : () -> T) : T {
//     f()
//   };
//   func u() : async () = async {};

//   func main() {
//     ignore call(func() = async { await u })
//   }
// }
