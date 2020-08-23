import Prim "mo:prim";
import Node "class:distributor/node";

// a distributed map from Nat to Text
// fixed number of nodes, dynamically installed on demand
actor a {

  type Key = Nat;
  type Value = Text;

  let n = 8;

  // would be nice if class import defined a type too
  type Node = actor {
    lookup : Key -> async ?Value;
    insert : (Key, Value) -> async ()
  };

  let nodes : [var ?Node] = Prim.Array_init(n, null);

  // would be nice if these were both tail calls on the platform
  public func lookup(k : Key) : async ?Value {
    switch (nodes[k % n]) {
      case null null;
      case (?n) await n.lookup(k);
    };
  };

  public func insert(k : Key, v : Value) : async () {
    let i = k % n;
    let node = switch (nodes[i]) {
      case null {
        let n = await Node(i); // dynamically install a new Node
        nodes[i] := ?n;
        n;
      };
      case (?n) n;
    };
    await node.insert(k, v);
  };

  public func go() : async () {
    var i = 0;
    while (i < 24) {
      let t = debug_show(i);
      assert (null == (await lookup(i)));
      await insert(i, t);
      assert (?t == (await lookup(i)));
      i += 1;
    };
  };

};

a.go() //OR-CALL ingress go "DIDL\x00\x00"
