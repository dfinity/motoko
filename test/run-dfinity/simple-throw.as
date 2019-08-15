// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.


async {
   print "1";
   try {
     print "2";
     throw (#error, "t2");
     assert(false);
   } {
     catch (#error, "t1") { assert false;};
     catch (#system, _ ) { assert false;};
     catch (#error,    _) {
       print "3";
     };
   };
   print "4";
   print "done";
};



