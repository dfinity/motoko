import Prim "mo:prim";

module {
    public let x = "MODULE VARIABLE";

    public func importedFunction() {
        Prim.debugPrint(x);
    }
}
