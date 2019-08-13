shared func t1() : async () {
   throw (#error, "t1");
   assert false;
};

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

shared func t4() : async () {
  try {
    try {
      throw (#error, "t4");
      assert(false);
    } {
      catch (#error, "t4") { throw (#error,"t41");};
      catch (#system, _ )  { assert false;};
      catch (#error, _ )   { assert true;};
    }
  } {
    catch (#error, "t41") { throw (#error,"t42");};
    catch (#system, _ )   { assert false;};
    catch (#error,  _ )   { assert true;};
  }
};


shared func t5() : async () {
  try {
    try {
      throw (#error, "t5");
      assert(false);
    } {
      catch (#system, _ )  { assert false;};
    }
  } {
    catch (#error, "t5") { throw (#error,"t51");};
    catch (#system, _ )   { assert false;};
    catch (#error,  _ )   { assert true;};
  }
};

async {

  try {
    await t1();
    assert false;
  } {
    catch (#error,"t1") { print ("\n t1 ok")};
    catch _ { assert false; }
  };

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

  try {
    await t4();
    assert false;
  } {
    catch (#error, "t42") { print ("\n t4 ok"); };
    catch _ { assert false; };
  };


  try {
    await t5();
    assert false;
  } {
    catch (#error, "t51") { print ("\n t5 ok"); };
    catch _ { assert false; };
  };

};

