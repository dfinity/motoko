import Prim "mo:prim";
import Migration "migration";

(with migration = Migration.run)
persistent actor {
    let message = "All " # "good";
    
    persistent func requiredFunction() {
        Prim.debugPrint("ENTER REQUIRED_FUNCTION VERSION 1");
        Prim.debugPrint(message);
        Prim.debugPrint("EXIT REQUIRED_FUNCTION VERSION 1");
    };

    var retained = requiredFunction;
    retained();
}
