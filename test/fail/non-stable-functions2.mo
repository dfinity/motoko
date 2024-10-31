//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
actor {
  func testFunc() {};

  stable var f = testFunc;

  let outer = func() { // flexible function
    func inner1() {}; // flexible function
    f := inner1; // invalid
  };
  
  func outer2() { // stable function
    func inner2() {}; // stable function
    f := inner2; // valid
  };
  
  func outer3() { //stable function
    let inner3 = func () {}; // flexible function
    f := inner3; // invalid
  };

  let outer4 = func() { // flexible function
    func middle4() { // flexible function
      func inner4() {}; // flexible function
      f := inner4; // invalid;
      f := middle4; // invalid;
      f := outer4; // invalid
    };
  };
  
  func outer5() { // stable function
    func middle5() { // stable function
      let inner5 = func () {}; // flexible function
      f := inner5; // invalid;
      f := middle5; // valid;
      f := outer5; // valid
    };
  };
};
