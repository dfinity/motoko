// @verify

actor Lits {
  func numLits() {
    let i : Int = 42;
    switch (i) {
      case 42 {};
      case 1000 {};
      case _ {};
    };

    let n : Nat = 42;
    switch (n) {
      case (42 : Nat) {};
      case 1000 {};
      case _ {};
    };
  };

  func boolLits() {
    let b = false;
    switch(b) {
      case true {};
      case false {};
    };
  }
}
