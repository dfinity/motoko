// returning an actor reference

actor {
  public query func foo() : async actor {} {
    actor "ic:C0FEFED00D41";
  };

  public query func bar() : async actor {} {
    actor ("ic:C0FEFED00D" # "CAFEBABEDEADBEEF84");
  }
}

//CALL query foo 0x4449444c0000
//CALL query bar 0x4449444c0000
