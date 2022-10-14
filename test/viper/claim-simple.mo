actor {

  var claimed = false;

  var count = 0 : Int;

  assert not (-1 == -1) and (-42 == -42) or true;
  //assert not claimed; // ctxt extension with "self" is missing

  public shared func claim() : async () {
    if (not claimed) {
      claimed := true;
      count := 1;
    };
  };

}