import Prim "mo:prim";

actor {
    func otherFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION");
    };

    stable let required = otherFunction;
    stable let optional = otherFunction;
    required();
    optional();
}
