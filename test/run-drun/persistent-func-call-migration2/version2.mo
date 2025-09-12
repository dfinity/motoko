import Prim "mo:prim";
import Migration "migration";
import Module2 "module2";

(with migration = Migration.run)
persistent actor {
    persistent func otherFunction() {
        Prim.debugPrint("Other function");
        Module2.importedFunction();
        Prim.debugPrint(Module2.x);
    };
    
    persistent func requiredFunction() {
        Prim.debugPrint("ENTER REQUIRED FUNCTION VERSION 1");
        otherFunction();
        Prim.debugPrint("EXIT REQUIRED FUNCTION VERSION 1");
    };

    var retained = requiredFunction;
    retained();
}
