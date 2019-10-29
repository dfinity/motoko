actor {

    import Stack "ListClient.mo";

    func main(): () {
        let st = Stack.empty();
        let newst = Stack.push(0, st);
    }
}
