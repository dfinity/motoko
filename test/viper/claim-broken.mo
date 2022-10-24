// @verify

actor {

  var claimed = false;

  var count = 0 : Int;

  assert:invariant not claimed implies count == 0;
  assert:invariant count == 0 or count == 1;

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
