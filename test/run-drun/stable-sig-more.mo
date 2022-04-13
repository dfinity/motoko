actor {

 public shared func f0() : async () { loop {} };
 stable let x0 = f0;

 public shared func f1(_ : Nat) : async Nat { loop {} };
 stable let x1 = f1;

 public shared func f2(_ : Nat, _ : Bool) : async (Nat, Bool) { loop {} };
 stable let x2 = f2;

 public shared func f3(_ : shared () -> async ()) : async (shared () -> async ()) { loop {} };
 stable let x3 = f3;

 public shared func f4(_ : shared () -> async ()) : async (shared () -> async ()) { loop {} };
 stable let x4 = f4;

 public shared func f5(_ : actor {}) : async (actor {}) { loop {} };
 stable let x5 = f5;

 public shared func f6(_ : shared () -> async actor {}) : async (shared () -> async actor {}) { loop {} };
 stable let x6 = f6;

}
