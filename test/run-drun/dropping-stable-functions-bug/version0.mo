import Prim "mo:prim";

persistent actor {

    stable func required() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 0");
    };

    stable func optional() {
        Prim.debugPrint("OPTIONAL FUNCTION VERSION 0");
    };

    required();
    optional();
}
