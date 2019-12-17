// returning an actor reference

actor {
  public query func foo() : async actor {} {
    actor "ic:C0FEFED00D41";
  }
}

//CALL query foo 0x4449444c0000
