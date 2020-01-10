import Stack "ListClient.mo";

actor {
    func main(): () {
        let st = Stack.empty();
        let newst = Stack.push(0, st);
    }
}
