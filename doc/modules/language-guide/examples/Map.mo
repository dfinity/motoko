import Array "mo:base/Array";
import Buckets "Buckets";

actor Map {

  let n = 8; // number of buckets

  type Key = Nat;
  type Value = Text;

  type Bucket = Buckets.Bucket;

  let buckets : [var ?Bucket] = Array.init(n, null);

  public func lookup(k : Key) : async ?Value {
    switch (buckets[k % n]) {
      case null null;
      case (?bucket) await bucket.lookup(k);
    };
  };

  public func insert(k : Key, v : Value) : async () {
    let i = k % n;
    let bucket = switch (buckets[i]) {
      case null {
        let n = await Buckets.Bucket(i); // dynamically install a new Bucket
        buckets[i] := ?n;
        n;
      };
      case (?bucket) bucket;
    };
    await bucket.insert(k, v);
  };

};
