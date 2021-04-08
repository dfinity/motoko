import Array "mo:base/Array";

import Principal "mo:base/Principal";
import Cycles "mo:base/ExperimentalCycles";
import Buckets "Buckets";

actor Map {

  let n = 8; // number of buckets

  type Key = Nat;
  type Value = Text;

  type Bucket = Buckets.Bucket;

  let buckets : [var ?Bucket] = Array.init(n, null);

  public func get(k : Key) : async ?Value {
    switch (buckets[k % n]) {
      case null null;
      case (?bucket) await bucket.get(k);
    };
  };

  public func put(k : Key, v : Value) : async () {
    let i = k % n;
    let bucket = switch (buckets[i]) {
      case null {
        Cycles.add(1_000_000_000_000); // add some cycles
        let b = await Buckets.Bucket(n, i); // dynamically install a new Bucket
        buckets[i] := ?b;
        b;
      };
      case (?bucket) bucket;
    };
    await bucket.put(k, v);
  };

  public func getPrincipalOfBucket(i : Nat) : async ?Principal {
    switch (buckets[i]) {
      case null null;
      case (?bucket) ? Principal.fromActor(bucket);
    };
  };


};
