// @verify

actor {

  var claimed = false;

  var count = 0 : Int;

  public shared func claim() : async () {
    if (not claimed) {
      await async {
        assert:1:async (claimed and count == 0);
        claimed := true;
        count += 1;
      };
    };
  };

}