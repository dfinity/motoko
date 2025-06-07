// moc lambdas.mo --package base $MOTOKO_BASE
import Array "mo:base/Array";
// import Iter "mo:base/Iter";

func map<T>(ar : [T], _f : T -> T) : [T] = ar;
// func forEach<T>(_ar : [T], _f : T -> ()) {};

func __main1(_szczebrzeszyn : [Nat]) : [Nat] {
  // let l = map(_szczebrzeszyn, func x = x + 1);
  map(_szczebrzeszyn, func(x : Nat) = x + 1);
  // Array.map<Nat, Nat>(ar, func x = x + 1);
};

func __main2(_szczebrzeszyn : [Nat]) : [Nat] {
  Array.map(_szczebrzeszyn, func(x : Nat) = x + 1);
};

func _regressionReturnUnit() {
  loop { if true return };
};

func _similarRegression<A <: ()>(a : A) {
  if true return;
  a : A;
};

func regressionRecursion(n : Nat) {
  if (n == 0) return;
  regressionRecursion(n - 1);
};

func regressionFunctionToAny1(_szczebrzeszyn : Nat) {
  if true {
    let _localFunction = func() {};
  } else ();
};
func regressionFunctionToAny2(_szczebrzeszyn : Nat) {
  if true {
    let _localFunction = func() {};
  };
};
func _useFunctionToAny() {
  // regressionFunctionToAny1(1);
  regressionFunctionToAny2(1);
};
func letReturnsFunction1() : () -> () {
  let _x = 1;
  let _f = func() {};
};
func letReturnsFunction2() : () {
  let _x = 1;
  let _f = func() {};
};
func letReturnsFunction() {
  let _x = 1;
  let _f = func() {};
};
func _useLetReturnsFunction() {
  ignore letReturnsFunction1();
  letReturnsFunction2();
  // letReturnsFunction(); // TODO: broken
  ();
};

func _m1() {
  let array1 = [1, 2, 3];
  // let array2 = Array.map(array1, func x = x * 2)
  //                                  ^
  // cannot infer type of variableMotoko(M0103)
};

func _m2() {
  let array1 = [1, 2, 3];
  // let array2 = Array.map(array1, func(x : Nat) = x * 2)
  //           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // cannot implicitly instantiate function of type
  //   <T, R>(array : [T], f : T -> R) -> [R]
  // to argument of type
  //   ([Nat], (x : Nat) -> ())
  // because no instantiation of T, R makes
  //   ([Nat], (x : Nat) -> ())  <:  (array : [T], f : T -> R)Motoko(M0098)
};

func _m3() {
  let array1 = [1, 2, 3];
  let array2 = Array.map(array1, func(x : Nat) : Nat = x * 2);
};

func _m3_2() {
  let array1 = [1, 2, 3];
  let array2 = Array.map<Nat, Nat>(array1, func x = x * 2);
};

func _n1() {
  let array1 = [1, 2, 3];
  // let array2 = array1 |> Iter.fromArray(_) |> Iter.map(_, func x = x + 1) |> Iter.toArray(_)
  // cannot infer type of variableMotoko(M0103)
};

func _n2() {
  let array1 = [1, 2, 3];
  // let array2 = array1 |> Iter.fromArray(_) |> Iter.map(_, func(x : Nat) = x + 1) |> Iter.toArray(_)
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
