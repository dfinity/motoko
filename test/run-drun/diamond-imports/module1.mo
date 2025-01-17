import Prim "mo:prim";
import Shared "shared-module";

module {
    public func testFunc() {
        Prim.debugPrint("MODULE 1");
    };

    public func getShared() : stable () -> () {
        Shared.testFunc;
    };
};
