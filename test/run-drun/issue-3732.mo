actor class TestAPI() {
  public func endpoint1(x: (Nat,Nat)) : async () {}; // compiler bug
  public func endpoint2(x:Nat, y: Nat) : async () {}; // works
  system func inspect({}) : Bool = true;
};