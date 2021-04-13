import Stack "ListClient";
import Prim "mo:⛔";
import Entry "entrypoint";

actor {
    func main(): () {
        let st = Stack.empty();
        let newst = Stack.push(0, st);
        let primTest: Word8 = Prim.natToWord8(5);
    }
}
