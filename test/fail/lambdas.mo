// moc lambdas.mo --package base $MOTOKO_BASE
// import Array "mo:base/Array";

// actor {
  func map<T>(ar : [T], f : T -> T) : [T] = ar; 

  func main(szczebrzeszyn : [Nat]): [Nat] {
    // map(szczebrzeszyn, func x = x + 1);
    map(szczebrzeszyn, func (x : Nat) = x + 1);
    // Array.map<Nat, Nat>(ar, func x = x + 1);
  }

  // func main() {
  //   1
  // }

  // func go1<K, T <: T -> T>(f : () -> T) : T = f();
  // func go2<T>(f : Nat -> T) : T = f(1);

  // func main() {
    // go1(func () = 1);
  // let x1 = go1<None, None>(func () { 1 });
  // let x2 = go2<Nat>(func x = x + 1);
  // }
// }