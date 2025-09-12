import Prim "mo:prim";

persistent actor {
    persistent func otherFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION");
    };

    let required = otherFunction;
    let optional = otherFunction;
    required();
    optional();
}
