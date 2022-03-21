import Prim "mo:⛔";

actor this {


   public func send_f0(
     f : shared (Nat) -> async Int
   ) : async () {
     Prim.debugPrint("ok");
   };

   public func f0(n : Nat) : async Int { 0 };

   public func f0_1(n : Int) : async Nat { 0 };

   public func f0_2() : async (Nat, Bool) { (0,true) };

   public func f0_3(i : Nat, on : ?Nat) : async (Int, ?Nat) { (0, null); };

   public func f0_4(ob : ?Bool) : async Int { 0 };

   public func f0_5(n : Nat) : async ?Bool { null };

   public func go() : async () {
      let t = debug_show (Prim.principalOfActor(this));

      // vanilla subtyping on in/out args
      do {
        let this = actor (t) : actor {
           send_f0 : (shared (n:Int) -> async Nat) -> async ();
        };
        try {
          await this.send_f0(f0_1);
        }
        catch e { Prim.debugPrint "wrong_0_1"; }
      };

      // vanilla subtyping on in/out arg sequences
      do {
        let this = actor (t) : actor {
           send_f0 : (shared () -> async (Nat,Bool)) -> async ();
        };
        try {
          await this.send_f0(f0_2);
        }
        catch e { Prim.debugPrint "wrong_0_2"; }
      };

      // opt subtyping in arg and return
      do {
        let this = actor (t) : actor {
           send_f0 : (shared (Nat, ?Nat) -> async (Int, ?Nat)) -> async ();
        };
        try {
          await this.send_f0(f0_3);
        }
        catch e { Prim.debugPrint "wrong_0_3"; }
      };

      // opt override in arg
      do {
        let this = actor (t) : actor {
           send_f0 : (shared (?Bool) -> async Int) -> async ();
        };
        try {
          await this.send_f0(f0_4);
        }
        catch e { Prim.debugPrint "wrong_0_4"; }
      };


      // opt override in return
      do {
        let this = actor (t) : actor {
           send_f0 : (shared (Nat) -> async ?Bool) -> async ();
        };
        try {
          await this.send_f0(f0_5);
        }
        catch e { Prim.debugPrint "wrong_0_5"; }
      };




   };


}
//SKIP run
//SKIP run-ir
//SKIP run-low
//CALL ingress go "DIDL\x00\x00"