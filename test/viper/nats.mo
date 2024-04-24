// @verify

actor Nats {

  var x = 42 : Nat;

  public func getZero() : async Nat {
    return 0;
  };

  public func getX() : async Nat {
    return x;
  };

  public func double() : async () {
    x := x * 2;
  };

  public func natToInt(n : Nat) : async Int {
    return n;
  };

  public func add(n : Nat, m : Int) : async Int {
    return n + m;
  };

};
