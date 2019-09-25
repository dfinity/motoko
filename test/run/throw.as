shared func t1() : async () {
   throw error("t1");
   assert false;
};

shared func t2() : async () {
   try {
     throw error("t2");
     assert(false);
   } catch e {
     switch (errorCode(e), errorMessage(e)) {
       case (#error, "t1") { assert false;};
       case (#system, _)   { assert false;};
       case (#error, _)    { assert true;};
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
        case (#error, "t3") { throw error("t31");};
        case (#system, _)   {assert false;};
        case (#error, _)    { assert false;};
      }
    }
  } catch e2 {
    switch (errorCode(e2), errorMessage(e2)) {
      case (#error, "t31") { assert true;};
      case (#system, _ )   { assert false;};
      case (#error, _)     { assert false;};
    }
  }
};

shared func t4() : async () {
  try {
    try {
      throw error("t4");
      assert(false);
    } catch e1 {
      switch (errorCode(e1), errorMessage(e1)) {
        case (#error, "t4") { throw error("t41");};
        case (#system, _)   { assert false;};
        case (#error, _)    { assert false;};
     }
    }
  } catch e2 {
    switch (errorCode(e2), errorMessage(e2)) {
      case (#error, "t41") { throw error("t42");};
      case (#system, _ )   { assert false;};
      case (#error,  _ )   { assert false;};
    }
  }
};


shared func t5() : async () {
  try {
    try {
      throw error("t5");
      assert(false);
    } catch e1 {
      switch (errorCode(e1), errorMessage(e1)) {
        case (#system, _ )  { assert false;};
	case _ { throw e1; }
      }
    }
  } catch e2 {
    switch (errorCode(e2), errorMessage(e2)) {
      case (#error, "t5") { throw error("t51");};
      case (#system, _ )  { assert false;};
      case (#error,  _ )  { assert false;};
    }
  }
};

async {

  try {
    await t1();
    assert false;
  } catch e {
    switch (errorCode(e), errorMessage(e)) {
      case (#error, "t1") { print ("\n t1 ok")};
      case _ { assert false; }
    }
  };

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

  try {
    await t4();
    assert false;
  } catch e {
    switch (errorCode(e), errorMessage(e)) {
      case (#error, "t42") { print ("\n t4 ok"); };
      case _ { assert false; };
    }
  };

  try {
    await t5();
    assert false;
  } catch e {
    switch (errorCode(e), errorMessage(e)) {
      case (#error, "t51") { print ("\n t5 ok"); };
      case _ { assert false; };
    }
  };

};
