import Prim "mo:⛔";
actor {
  var count = 0;
  public func inc() : async () {
    count += 1;
  };

  public func repeat() : async Nat {
    loop {
      ignore inc();
    }
  };

  public func repeatUntil() : async Nat {
    var sent = 0;
    try {
      loop {
       ignore inc();
       sent += 1
      }
    } catch (e) {
      Prim.debugPrint(debug_show {sent});
      return sent
    }
  };

  public func get() : async Nat {
    Prim.debugPrint(debug_show {count});
    return count;
  }

}
//SKIP ic-ref-run
//CALL ingress repeat RElETAAA
//CALL ingress get RElETAAA
//CALL ingress repeatUntil RElETAAA
//CALL ingress get RElETAAA
