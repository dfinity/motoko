import Prim "mo:prim";
actor class Empty() {

  Prim.debugPrint("empty");

  public func test() : async () {
    Prim.debugPrint(debug_show () # " tested");
  };
}
