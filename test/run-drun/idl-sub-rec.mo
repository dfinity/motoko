import Prim "mo:⛔";
// test candid subtype check on recursive types
actor this {

  type List<T> = {
    #nil;
    #left:(T,List<T>)
  };

  type Tree<T> = {
    #nil;
    #left: (T, Tree<T>);
    #right: (T, Tree<T>)
  };

  public func f0(l : List<Nat>) : async Tree<Nat> { l };
  public func f1(l : List<Nat>) : async Tree<Int> { l };
  public func f2(l : List<Int>) : async Tree<Nat> { #nil };
  public func f3(l : Tree<Int>) : async List<Nat> { #nil };
  public func f4(l : Any) : async None { Prim.trap "bail" };

  public func send_f0(
    f : shared List<Nat> -> async Tree<Int>
  ) : async () {
    Prim.debugPrint("ok 0");
  };

  public func send_f1(
    a : [shared List<Nat> -> async Tree<Int>]
  ) : async () {
    Prim.debugPrint("ok 0");
  };

  func tabulate<T>(n : Nat, v : T) : [T] {
    Prim.Array_tabulate(n, func (_ : Nat) : T { v });
  };

  public func go() : async () {
    await this.send_f0(f0);
    await this.send_f0(f1);
    await this.send_f0(f2);
    await this.send_f0(f3);
    await this.send_f0(f4);

    // test vectors, should benefit from memoization
    await this.send_f1(tabulate(1024, f0));
    await this.send_f1(tabulate(1024, f1));
    await this.send_f1(tabulate(1024, f2));
    await this.send_f1(tabulate(1024, f3));
    await this.send_f1(tabulate(1024, f4));
  };
}
//SKIP run
//SKIP run-ir
//SKIP run-low
///SKIP comp-ref
//CALL ingress go "DIDL\x00\x00"