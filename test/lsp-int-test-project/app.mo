import Stack "ListClient";
import Prim "mo:⛔";
import Entry "entrypoint";

actor {
    func main(): () {
        let st = Stack.empty();
        let newst = Stack.push(0, st);
        let primTest: Nat8 = Prim.natToNat8(5);
    }
}
