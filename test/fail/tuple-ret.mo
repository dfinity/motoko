import Prim  "mo:prim";

actor {

  module Array {
    public func map<T, U>(self : [T], f : T -> U) : [U] {
       Prim.Array_tabulate<U>(self.size(), func (i) { f(self[i]) });
    }
  };

  func update0(members : [(Nat, Nat)]) {
    let _ = members.map(
      func(memberRole) {
        memberRole
      }
    )
  };

  func update1(members : [(Nat, Nat)]) {
    let _ = members.map<(Nat, Nat), (Nat, Nat)>(
      // accepted, but should be rejected I think (see update1b)
      func(memberRole : (Nat, Nat)) : (Nat, Nat) {
        memberRole
      }
    )
  };

/*
  func update1b(members : [(Nat, Nat)]) {
    // same as update1 but with named function argument
    func f(memberRole : (Nat, Nat)) : (Nat, Nat) {
        memberRole
    };
    let _ = members.map<(Nat, Nat), (Nat, Nat)>(
      f //rejected, but just a refactor of update1!
    )
  };
*/

  func update2(members : [(Nat, Nat)]) {
    let _ = members.map<(Nat, Nat), (Nat, Nat)>(
      func(memberRole) {
        memberRole
      }
    )
  };

/*
  func update3(members : [(Nat, Nat)]) {
    // Does not type check (as expected)
    let _ = members.map(
      func(memberRole : (Nat, Nat)) : (Nat, Nat) {
        memberRole
      }
    )
  }
*/

  func update4(members : [(Nat, Nat)]) {
    let _ = members.map(
      func(memberRole : (Nat, Nat)) : ((Nat, Nat)) {
        memberRole
      }
    )
  };

  func update4b(members : [(Nat, Nat)]) {
    // same as update1 but with named function argument
    func f(memberRole : (Nat, Nat)) : ((Nat, Nat)) {
        memberRole
    };
    let _ = members.map<(Nat, Nat), (Nat, Nat)>(
      f //accepted, and just a refactor of update4
    )
  };

  func update5(members : [(Nat, Nat)]) {
    let _ = members.map(
      func(memberRole : (Nat, Nat)) : (ret: (Nat, Nat)) {
        memberRole
      }
    )
  };

  func update6(members : [(Nat, Nat)]) {
    let _ = members.map(
      func((n1 : Nat, n2 : Nat)) : ((Nat, Nat)) {
        (n1, n2)
      }
    )
  };

}
