/// Mutable hash map (aka Hashtable)
///
/// This module defines an imperative hash map (hash table), with a general key and value type.
///
/// It has a minimal object-oriented interface: `get`, `set`, `delete`, `count` and `entries`.
///
/// The class is parameterized by the key's equality and hash functions,
/// and an initial capacity.  However, as with the `Buffer` class, no array allocation
/// happens until the first `set`.
///
/// Internally, table growth policy is very simple, for now:
///  Double the current capacity when the expected bucket list size grows beyond a certain constant.

import Prim "mo:⛔";
import P "Prelude";
import A "Array";
import Hash "Hash";
import Iter "Iter";
import AssocList "AssocList";
import Nat32 "Nat32";

module {


  // hash field avoids re-hashing the key when the array grows.
  type Key<K> = (Hash.Hash, K);

  // key-val list type
  type KVs<K, V> = AssocList.AssocList<Key<K>, V>;

  /// An imperative HashMap with a minimal object-oriented interface.
  /// Maps keys of type `K` to values of type `V`.
  public class HashMap<K, V>(
    initCapacity : Nat,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash) {

    var table : [var KVs<K, V>] = [var];
    var _count : Nat = 0;

    /// Returns the number of entries in this HashMap.
    public func size() : Nat = _count;

    /// Deletes the entry with the key `k`. Doesn't do anything if the key doesn't
    /// exist.
    public func delete(k : K) = ignore remove(k);

    func keyHash_(k : K) : Key<K> = (keyHash(k), k);

    func keyHashEq(k1 : Key<K>, k2 : Key<K>) : Bool { k1.0 == k2.0 and keyEq(k1.1, k2.1) };

    /// Removes the entry with the key `k` and returns the associated value if it
    /// existed or `null` otherwise.
    public func remove(k : K) : ?V {
      let m = table.size();
      if (m > 0) {
        let h = Prim.nat32ToNat(keyHash(k));
        let pos = h % m;
        let (kvs2, ov) = AssocList.replace<Key<K>, V>(table[pos], keyHash_(k), keyHashEq, null);
        table[pos] := kvs2;
        switch(ov){
          case null { };
          case _ { _count -= 1; }
        };
        ov
      } else {
        null
      };
    };

    /// Gets the entry with the key `k` and returns its associated value if it
    /// existed or `null` otherwise.
    public func get(k : K) : ?V {
      let h = Prim.nat32ToNat(keyHash(k));
      let m = table.size();
      let v = if (m > 0) {
        AssocList.find<Key<K>, V>(table[h % m], keyHash_(k), keyHashEq)
      } else {
        null
      };
    };

    /// Insert the value `v` at key `k`. Overwrites an existing entry with key `k`
    public func put(k : K, v : V) = ignore replace(k, v);

    /// Insert the value `v` at key `k` and returns the previous value stored at
    /// `k` or `null` if it didn't exist.
    public func replace(k : K, v : V) : ?V {
      if (_count >= table.size()) {
        let size =
          if (_count == 0) {
            if (initCapacity > 0) {
              initCapacity
            } else {
              1
            }
          } else {
            table.size() * 2;
          };
        let table2 = A.init<KVs<K, V>>(size, null);
        for (i in table.keys()) {
          var kvs = table[i];
          label moveKeyVals : ()
          loop {
            switch kvs {
              case null { break moveKeyVals };
              case (?((k, v), kvsTail)) {
                let pos2 = Nat32.toNat(k.0) % table2.size(); // critical: uses saved hash. no re-hash.
                table2[pos2] := ?((k,v), table2[pos2]);
                kvs := kvsTail;
              };
            }
          };
        };
        table := table2;
      };
      let h = Prim.nat32ToNat(keyHash(k));
      let pos = h % table.size();
      let (kvs2, ov) = AssocList.replace<Key<K>, V>(table[pos], keyHash_(k), keyHashEq, ?v);
      table[pos] := kvs2;
      switch(ov){
        case null { _count += 1 };
        case _ {}
      };
      ov
    };

    /// An `Iter` over the keys.
    public func keys() : Iter.Iter<K>
    { Iter.map(entries(), func (kv : (K, V)) : K { kv.0 }) };

    /// An `Iter` over the values.
    public func vals() : Iter.Iter<V>
    { Iter.map(entries(), func (kv : (K, V)) : V { kv.1 }) };

    /// Returns an iterator over the key value pairs in this
    /// `HashMap`. Does _not_ modify the `HashMap`.
    public func entries() : Iter.Iter<(K, V)> {
      if (table.size() == 0) {
        object { public func next() : ?(K, V) { null } }
      }
      else {
        object {
          var kvs = table[0];
          var nextTablePos = 1;
          public func next () : ?(K, V) {
            switch kvs {
              case (?(kv, kvs2)) {
                kvs := kvs2;
                ?(kv.0.1, kv.1)
              };
              case null {
                if (nextTablePos < table.size()) {
                  kvs := table[nextTablePos];
                  nextTablePos += 1;
                  next()
                } else {
                  null
                }
              }
            }
          }
        }
      }
    };

  };

  /// clone cannot be an efficient object method,
  /// ...but is still useful in tests, and beyond.
  public func clone<K, V> (
    h : HashMap<K, V>,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash
  ) : HashMap<K, V> {
    let h2 = HashMap<K, V>(h.size(), keyEq, keyHash);
    for ((k,v) in h.entries()) {
      h2.put(k,v);
    };
    h2
  };

  /// Clone from any iterator of key-value pairs
  public func fromIter<K, V>(
    iter : Iter.Iter<(K, V)>,
    initCapacity : Nat,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash
  ) : HashMap<K, V> {
    let h = HashMap<K, V>(initCapacity, keyEq, keyHash);
    for ((k, v) in iter) {
      h.put(k, v);
    };
    h
  };

  public func map<K, V1, V2>(
    h : HashMap<K, V1>,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash,
    mapFn : (K, V1) -> V2,
  ) : HashMap<K, V2> {
    let h2 = HashMap<K, V2>(h.size(), keyEq, keyHash);
    for ((k, v1) in h.entries()) {
      let v2 = mapFn(k, v1);
      h2.put(k, v2);
    };
    h2
  };

  public func mapFilter<K, V1, V2>(
    h : HashMap<K, V1>,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash,
    mapFn : (K, V1) -> ?V2,
  ) : HashMap<K, V2> {
    let h2 = HashMap<K, V2>(h.size(), keyEq, keyHash);
    for ((k, v1) in h.entries()) {
      switch (mapFn(k, v1)) {
        case null { };
        case (?v2) {
          h2.put(k, v2);
        };
      }
    };
    h2
  };

}
