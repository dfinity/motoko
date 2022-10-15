actor {

  var claimed = false;

  var count = 0 : Int;

  assert claimed and not (-1 == -1) and (-42 == -42) or true;
  assert count > 0;

  public shared func claim() : async () {
  };

}