actor {

   var c = 0;

   public func inc() : async () { c += 1 };
   public func set(n : Nat) : async () { c := n };
   public query func read() : async Nat { c };
   public func reset() : () { c := 0 }; // oneway

   system func inspect_message(
     { caller : Principal;
       arg : Blob;
       msg : {
         #inc : Any;
         #set : () -> Nat;
         #read : Any;
         #reset : () -> ();
      }
    }
    ) : Bool  {
      if (arg.size() > 512) return false;
      switch (msg) {
         case (#inc _) { true };
         case (#read _) { true };
         case (#set n)  { (n()) >= 0 };
         case (#reset f) { true };
      }
     }
};
