import Prim "mo:prim";
import Migration "migration";

(with migration = Migration.run)
persistent actor {

    let uninitialized = "I'm" # "crashing";
    func requiredFunction() {
        Prim.debugPrint("ENTER REQUIRED_FUNCTION VERSION 1");
        Prim.debugPrint(uninitialized);
        Prim.debugPrint("EXIT REQUIRED_FUNCTION VERSION 1");
    };

    // Need to keep this function, as it may be still called by migration logic!
    func optionalFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION VERSION 1");
    };
    var retained = optionalFunction;

    retained := requiredFunction;
    retained();
}
