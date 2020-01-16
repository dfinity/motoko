import Stack "ListClient.mo";
import Prim "mo:prim";
import Entry "entrypoint.mo";

actor {
    func main(): () {
        let st = Stack.empty();
        let newst = Stack.push(0, st);
        let primTest: Word8 = Prim.natToWord8(5);
    }
}
