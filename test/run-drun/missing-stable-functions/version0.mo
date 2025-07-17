import Prim "mo:prim";

actor {
    func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION");
    };

    func optionalFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION");
    };

    stable let required = requiredFunction;
    stable let optional = optionalFunction;
    required();
    optional();
}
