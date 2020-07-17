// returning an actor reference

actor {
  public query func foo() : async actor {} {
    actor "ic:bfozs-kwa73-7nadi";
  };

  public query func bar() : async actor {} {
    actor ("ic:lsoqw-g6a73-7na" # "do-k725l-5xvnx-3xq");
  }

}

//CALL query foo 0x4449444c0000
//CALL query bar 0x4449444c0000
