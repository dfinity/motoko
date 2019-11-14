// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.

async {
   Debug.print "1";
   try {
     Debug.print "2";
     throw error("t2");
     assert(false);
   } catch e {
     switch (errorCode(e), errorMessage(e)) {
       case (#error, "t1") { assert false; };
       case (#system, _ ) { assert false; };
       case (#error, _) { Debug.print "3"; };
     }
   };
   Debug.print "4";
   Debug.print "done";
};


