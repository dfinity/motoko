import P "mo:prim";

actor a {

  var count : Nat = 0;

  public func opt() : async ?Text {
    do ? {
      await opt() !;
    }
  };

  public func go() {
    await opt();
  };
}
//CALL ingress go "DIDL\x00\x00"
