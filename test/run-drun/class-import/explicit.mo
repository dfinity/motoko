import Prim "mo:prim";
// test that modules containing a single, public class can be imported.
module {
  public actor class C() {
    Prim.debugPrint("explicit");

    public func test() : async () {
      Prim.debugPrint("explict tested");
    };
  }
}
