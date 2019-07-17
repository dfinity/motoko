import S "server.as";

actor class Client() = this {
  // TODO: these should be constructor params once we can compile them
  var name : Text = "";
  var server : ?S.Server  = null;

  public func go(n : Text, s : S.Server) {
    name := n;
    server := ?s;
    ignore(async {
      let sub = await s.subscribe(this.send);
      sub.post("hello from " # name);
      sub.post("goodbye from " # name);
      sub.cancel();
    })
  };

  public func send(msg : Text) {
    print(name # " received " # msg # "\n");
  };
};

