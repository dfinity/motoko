import Prim "mo:prim";
import Shared "shared-module";

module {
    public persistent func testFunc() {
        Prim.debugPrint("MODULE 2");
    };

    public func getShared() : persistent () -> () {
        Shared.testFunc;
    };
};
