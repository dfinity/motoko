import Prim "mo:prim";
import _StableSet "StableSet";
import {type Order; type StableSet; StableSet } "StableSet";

persistent actor {

  var version = 0;
  version += 1;

  Prim.debugPrint(debug_show {version});

  // c.f. persistent
  stable func inc(i : Int, j : Int) : Order {
     if (i < j) #less else if (i == j) #equal else #greater
  };

  var s = StableSet<Int, inc>(inc);
  s.add(version);

  do {
    var i = version;
    while (i > 0) {
      assert s.mem(i);
      Prim.debugPrint(debug_show {i});
      i -= 1;
    }
  }
};
