// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.

shared func t2() : async () {
   try {
     throw error("t2");
     assert(false);
   } catch e {
        switch (errorCode(e),errorMessage(e)) {
          case (#error, "t2") { };
          case (#system, _ ) { assert false;};
          case (#error, _) { assert false;};
     }
   }
};

shared func t3() : async () {
  try {
    try {
      throw error("t3");
      assert(false);
    } catch e1 {
      switch (errorCode(e1), errorMessage(e1)) {
        case (#error, "t3") {
          throw error("t31");
        };
        case (#system, _) {
          assert false;
        };
        case (#error, _) {
          assert false;
        };
      }
    }
  }
  catch e2 {
    switch (errorCode(e2),errorMessage(e2)) {
     case (#error, "t31") { };
     case (#system, _) {
       assert false;
     };
     case (#error, _) {
       assert true;
     };
    }
  }
};


async {

  try {
    await t2();
    print ("\n t2 ok");
  } catch _ {
    assert false;
  };

  try {
    await t3();
    print ("\n t3 ok");
  } catch _ {
    assert false;
  };

};

