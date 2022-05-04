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
         #__get_candid_interface_tmp_hack: Any;
         #__motoko_async_helper: Any;
         #__motoko_stable_var_size: Any;
      }
    }
    ) : Bool  {
      if (arg.size() > 512) return false;
      switch (msg) {
         case (#inc _) { true };
         case (#read _) { true };
         case (#set n)  { (n()) != 666 };
         case (#reset f) { false };
         case (#__get_candid_interface_tmp_hack _) { true };
         case _ { false };
      }
     }
};
