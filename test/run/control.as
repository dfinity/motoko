actor class Control() {

  private condition() : Bool = false;

  testBlock() {
    label l1 {
      break l1
    };
    label l2 break l2();
    label l2 (let m = 1 + (break l2) : Int);
    let n = label l3 : Int { break l3(2) };
    let (x, y, z) = label l3 : (Int, Bool, Text) (break l3(2, true, ""));
  };

  testWhile() {
    label l while (condition()) {
      if true break l
      else continue l;
    };
  };

  testLoop() {
    label l loop {
      if true break l
      else continue l;
    };
  };

  testLoopWhile() {
    label l loop {
      if true break l
      else continue l;
    } while (condition());
  };

/*
  testLoopWhile2() {
    loop { } while (false);
  };

  testLoopWhile3() {
    label l {
      loop { }
      while (false and true)
    };
  };

  testLoopWhile4() {
    label l loop {
    } while (true and false);
  };
*/

  testNestedWhile() {
    label l while (condition()) {
      if true break l
      else continue l;
      label m while (condition()) {
        if true continue l
        else break m;
      };
    };
  };
};
