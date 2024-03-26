//MOC-FLAG --package base $MOTOKO_BASE
import {fromActor; toText} = "mo:base/Principal";
import {print} = "mo:base/Debug";
actor New {

   public func f(a: ?Nat) : async (?Nat) {
     print(debug_show ({ a }));
     a;
   };

   public func test() : async (?Nat, ?Nat) {

     // Coerce actor to previous interface and see if its arguments deserialize to defaults
     let Old = actor (toText (fromActor(New))) : actor { f: () ->  async ?Nat };

     (await New.f(?666), await Old.f());

   }

}
//SKIP run
//SKIP run-ir
//SKIP run-low
//CALL ingress test 0x4449444C0000