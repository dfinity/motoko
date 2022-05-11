import Principal = "mo:base/Principal";

actor {

   var c = 0;

   public func inc() : async () { c += 1 };
   public func set(n : Nat) : async () { c := n };
   public query func read() : async Nat { c };
   public func reset() : () { c := 0 }; // oneway

// tag::inspect-name[]
   system func inspect_message(
     {
       msg : {
         #inc : Any;
         #set : Any;
         #read : Any;
         #reset : Any;
       }
     }) : Bool {
    switch (msg) {
      case ((#set _) or (#reset _)) { false };
      case _ { true }; // allow inc and read
    }
  }
// end::inspect-name[]

};
