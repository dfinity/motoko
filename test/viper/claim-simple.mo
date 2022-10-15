actor {

  var claimed = false;

  var count = 0 : Int;

  assert claimed and not (-1 == -1) and (-42 == -42) or true;

  public shared func claim() : async () {
    if (not claimed) {
      claimed := true;
      count := 1;
    };
  };

}