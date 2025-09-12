//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
persistent actor {
  persistent func testFunc() {};

  var f = testFunc;

  func outer() { // transient function
    persistent func inner1() {}; // persistent function in transient scope
    f := inner1; // invalid
  };
  
  persistent func outer2() { // persistent function
    persistent func inner2() {}; // persistent function
    f := inner2; // valid
  };
  
  persistent func outer3() { // persistent function
    func inner3() {}; // transient function
    f := inner3; // invalid
  };

  func outer4() { // transient function
    persistent func middle4() { // persistent function in transient scope
      persistent func inner4() {}; // persistent function in transient scope
      f := inner4; // invalid;
      f := middle4; // invalid;
      f := outer4; // invalid
    };
  };
  
  persistent func outer5() { // persistent function
    persistent func middle5() { // persistent function
      func inner5() {}; // transient function
      f := inner5; // invalid;
      f := middle5; // valid;
      f := outer5; // valid
    };
  };
};
