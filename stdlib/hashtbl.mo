import P "prelude.mo";
import Hash "hash.mo";
import AssocList "assocList.mo";

module {

/**

Hash tables
===============

This module defines imperative hash tables, with general key and value types.

It has a minimal object-oriented interface: get, set and iter.

The class is parameterized by the key's equality and hash functions,
and an initial capacity.  However, as with `Buf`, no array allocation
happens until the first `set`.

Internally, table growth policy is very simple, for now:
  Double an initial capacity when the expected
  bucket list beyond a certain constant.

*/

// key-val list type
public type KVs<K,V> = AssocList.AssocList<K,V>;

public class Hashtbl<K,V> (
  initCapacity: Nat,
  keyEq: (K,K) -> Bool,
  keyHash: K -> Hash.Hash) {

  var table : [var KVs<K,V>] = [var];
  var count : Nat = 0;

  public func get(k:K) : ?V {
    let h = word32ToNat(keyHash(k));
    let m = table.len();
    let v = if (m > 0) {
      AssocList.find<K,V>(table[h % m], k, keyEq)
    } else {
      null
    };
  };

  public func set(k:K, v:V) : ?V {
    if (count >= table.len() * 4) {
      let size =
        if (count == 0)
          initCapacity
        else table.len() * 2;
      let table2 = Array_init<KVs<K,V>>(size, null);
      for (i in table.keys()) {
        table2[i] := table[i];
      };
      table := table2;
    };
    let h = word32ToNat(keyHash(k));
    let pos = h % table.len();
    let (kvs2, ov) = AssocList.replace<K,V>(table[pos], k, keyEq, ?v);
    table[pos] := kvs2;
    switch(ov){
    case null { count += 1 };
    case _ {}
    };
    ov
  };

  public func iter() : Iter<(K,V)> {
    if (table.len() == 0) {
      object { public func next() : ?(K,V) { null } }
    }
    else {
      object {
        var kvs = table[0];
        var nextTablePos = 1;
        public func next () : ?(K,V) {
          switch kvs {
          case (?(kv, kvs2)) {
                 kvs := kvs2;
                 ?kv
               };
          case null {
                 if (nextTablePos < table.len()) {
                   kvs := table[nextTablePos];
                   nextTablePos += 1;
                   next()
                 } else {
                   null
                 }
               }
          }
        };
      }
    }
  };
};
}
