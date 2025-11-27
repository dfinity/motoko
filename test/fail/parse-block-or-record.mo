//MOC-FLAG --error-recovery
func _m1() {
  let _r = { var x = 1 }; // record with var field
  let _b = do { var x = 1 }; // block with var definition, same syntax...
  switch 1 {
    case _ { var x = 1 } // ambiguous, block preferred
  }
};

func _m2() {
  let _r = { x = 1 }; // record with value field
  // let _b = do { x = 1 }; // syntax error
  switch 1 {
    case _ { x = 1 }; // ambiguous, block preferred, syntax error
    case _ { x = 1    // also missing '}'
  }
};

func _var0() {
  switch 1 {
    case _ { y = 2; z = 3; w = 4 }
  }
};
func _var1() {
  switch 1 {
    case _ { var x = 1; y = 2; z = 3; w = 4 }
  }
};
func _var2() {
  switch 1 {
    case _ { var x = 1; var y = 2; z = 3; w = 4 }
  }
};

func _hasSyntaxError() {
  ;
};

func _mWith() {
  let r = { x = 1 };
  let _r = { r with x = 1 }; // record update syntax
  let _b = do { r with x = 1 }; // syntax error
  switch 1 {
    case _ { r with x = 1 } // ambiguous, block preferred, syntax error
  }
};

func mcatch() : async () {
  try await mcatch()
  catch _ { var x = 1; y = 2; z = 3; w = 4 }
  finally { var x = 1; y = 2; z = 3; w = 4 };

  throw { var x = 1; y = 2; z = 3; w = 4 };
};

func _letFail() {
  let ?x = ?1 else { var x = 1; y = 2; z = 3; w = 4 };
};

func _conds() {
  if true { var x = 1; y = 2; z = 3; w = 4 }
  else { var x = 1; y = 2; z = 3; w = 4 };

  while true { var x = 1; y = 2; z = 3; w = 4 };

  loop { var x = 1; y = 2; z = 3; w = 4 };
};

func _weird() : async () {
  ignore { var x = 1; y = 2; z = 3; w = 4 };
  async { var x = 1; y = 2; z = 3; w = 4 };
};

module ActuallyBlock {
  func _letFirst() {
    let x = 1;
    x = 2;
    ignore x;
  };
  func _varFirst() {
    var x = 1;
    x = 2;
    ignore x;
  };
  func _base() {
    x = 2; // Missing let/var? Or assignment := operator? Or meant a record?
    ignore x; // Actually a block
  }
}