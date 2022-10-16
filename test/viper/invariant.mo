actor {

  var claimed = false;

  var count = 0 : Int;

  assert claimed and not (-1 >= count) and (-42 < count) or true;
  assert count > 0;

  public shared func claim() : async () {
  };

}
