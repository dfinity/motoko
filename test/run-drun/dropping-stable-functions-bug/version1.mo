import Prim "mo:prim";
import Migration "migration";

(with migration = Migration.run)
persistent actor {

    let uninitialized = "I'm" # "crashing";
    stable func required() {
        Prim.debugPrint("ENTER REQUIRED_FUNCTION VERSION 1");
        Prim.debugPrint(uninitialized);
        Prim.debugPrint("EXIT REQUIRED_FUNCTION VERSION 1");
    };

    // Need to keep this function, as it may be still called by migration logic!
    stable func optional() {
        Prim.debugPrint("OPTIONAL FUNCTION VERSION 1");
    };
    var retained = required;
    retained();
}
