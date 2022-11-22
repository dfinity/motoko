#!/usr/bin/env bash

# Tests that `moc` can output Viper code
(cat <<__END__
actor {

  var claimed = false;

  var count = 0 : Int;

  assert:invariant count == 0 or count == 1;
  assert:invariant not claimed implies count == 0;

  public shared func claim() : async () {
    if (not claimed) {
      claimed := true;

      await async {
        assert:1:async (claimed and count == 0);
        count += 1;
      };
    };
  };

}
__END__
) | moc --viper /dev/stdin
