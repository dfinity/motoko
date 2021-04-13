import Prim "mo:⛔";
actor class Empty() {

  Prim.debugPrint("empty");

  public func test() : async () {
    Prim.debugPrint(debug_show () # " tested");
  };
}
