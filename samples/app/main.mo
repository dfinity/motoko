import Server "server";
import Client "client";

let server = Server.Server();
let bob = Client.Client();
let alice = Client.Client();
let charlie = Client.Client();
bob.go("bob", server);
alice.go("alice", server);
charlie.go("charlie", server);
