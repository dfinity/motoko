// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.

shared func t2() : async () {
   try {
     throw (#error, "t2");
     assert(false);
   } {
     catch (#error, "t1") { assert false;};
     catch (#system, _ ) { assert false;};
     catch (#error,    _) { assert true;};
   }
};

shared func t3() : async () {
  try {
    try {
      throw (#error, "t3");
      assert(false);
    } {
      catch (#error, "t3") { throw (#error,"t31");};
      catch (#system, _ ) { assert false;};
      catch (#error,    _) { assert true;};
    }
  }
  {
     catch (#error, "t31") { assert true;};
     catch (#system, _ ) { assert false;};
     catch (#error,    _) { assert true;};
  }
};


async {

  try {
    await t2();
    print ("\n t2 ok");
  } {
    catch _ { assert false; };
  };
  try {
    await t3();
    print ("\n t3 ok");
  } {
    catch _ { assert false; };
  };


};

