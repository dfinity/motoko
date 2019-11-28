// This file only exercises local throws that don't cross function boundaries.
actor a {
  public func go() = ignore async {
     debugPrint "1";
     try {
       debugPrint "2";
       throw error("t2");
       assert(false);
     } catch e {
       switch (errorCode(e), errorMessage(e)) {
         case (#error, "t1") { assert false; };
         case (#system, _ ) { assert false; };
         case (#error, _) { debugPrint "3"; };
       }
     };
     debugPrint "4";
     debugPrint "done";
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
