// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.

async {
   print "1";
   try {
     print "2";
     throw error("t2");
     assert(false);
   } catch e {
     switch (errorCode(e), errorMessage(e)) {
       case (#error, "t1") { assert false; };
       case (#system, _ ) { assert false; };
       case (#error, _) { print "3"; };
     }
   };
   print "4";
   print "done";
};


