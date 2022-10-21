actor {

  var claimed = false;

  var count = 0 : Int;

  public shared func claim() : async () {
    if (not claimed) {
      claimed := true;
      count += 1;
    };
  };

}
