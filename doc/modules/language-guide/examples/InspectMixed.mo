import Principal = "mo:base/Principal";

actor {

   var c = 0;

   public func inc() : async () { c += 1 };
   public func set(n : Nat) : async () { c := n };
   public query func read() : async Nat { c };
   public func reset() : () { c := 0 }; // oneway

// tag::inspect-mixed[]
   system func inspect_message(
     {
       caller : Principal;
       arg : Blob;
       msg : {
         #inc : Any;
         #set : () -> Nat;
         #read : Any;
         #reset : Any;
       }
     }) : Bool {
    if (Principal.isAnonymous(caller)) return false;
    if (arg.size() > 512) return false;
    switch (msg) {
      case (#set n) { n() != 13 };
      case (#reset _) { false };
      case _ { true }; // allow inc and read
    }
  }
// end::inspect-mixed[]
};
