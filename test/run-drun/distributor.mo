import Prim "mo:prim";
import Node "distributor/node";

// A naive, distributed map from Nat to Text.
// Illustrates dynamic installation of imported actor classes.
// Uses a fixed number of nodes, dynamically installed on demand.
actor a {

  type Key = Nat;
  type Value = Text;

  // Number of Nodes
  let n = 8;

  // Would be nice if class import defined a type too
  type Node = actor {
    lookup : Key -> async ?Value;
    insert : (Key, Value) -> async ()
  };

  let nodes : [var ?Node] = Prim.Array_init(n, null);

  // Would be nice if these were both tail calls on the platform
  public func lookup(k : Key) : async ?Value {
    switch (nodes[k % n]) {
      case null null;
      case (?node) await node.lookup(k);
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
      case (?node) node;
    };
    await node.insert(k, v);
  };

  // Test
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
