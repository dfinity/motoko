/**
[#mod-TrieMap]
= `TrieMap` -- Functional map
*/

import T "Trie";
import P "Prelude";
import I "Iter";
import Hash "Hash";
import List "List";

/*

Trie Map
=========================

This module defines an imperative hash map, with a general key and value type.  It matches the interface and semantics of HashMap.  Unlike HashMap, its internal representation uses a functional hash trie (see `trie.mo`).

This class permits us to compare the performance of two representations of hash-based maps, where tries (as binary trees) permit more efficient, constant-time, cloning compared with ordinary tables.  This property is nice for supporting transactional workflows where map mutations may be provisional, and where we may expect some mutations to be uncommitted, or to "roll back".

For now, this class does not permit a direct `clone` operation (neither does `HashMap`), but it does permit creating iterators via `iter()`.  The effect is similar: Each iterator costs `O(1)` to create, but represents a fixed view of the mapping that does not interfere with mutations (it will _not_ view subsequent insertions or mutations, if any).

*/

module {
public class TrieMap<K,V> (isEq:(K, K) -> Bool, hashOf: K -> Hash.Hash) {

  var map = T.empty<K, V>();
  var _count : Nat = 0;

  public func count() : Nat = _count;

  public func set(k:K, v:V) : ?V {
    let keyObj = {key=k; hash=hashOf(k);};
    let (map2, ov) =
      T.insert<K,V>(map, keyObj, isEq, v);
    map := map2;
    switch(ov){
    case null { _count += 1 };
    case _ {}
    };
    ov
  };

  public func get(k:K) : ?V = {
    let keyObj = {key=k; hash=hashOf(k);};
    T.find<K,V>(map, keyObj, isEq)
  };

  public func del(k:K) : ?V = {
    let keyObj = {key=k; hash=hashOf(k);};
    let (t, ov) = T.remove<K, V>(map, keyObj, isEq);
    map := t;
    switch(ov){
    case null { _count -= 1 };
    case _ {}
    };
    ov
  };

  // notably, each iterator gets a _persistent view_ of the mapping,
  // by virtue of the trie being a persistent data structure.
  public func iter() : I.Iter<(K,V)> = object {
    var stack = ?(map, null) : List.List<T.Trie<K,V>>;
    public func next() : ?(K,V) {
      switch stack {
      case null { null };
      case (?(trie, stack2)) {
        switch trie {
        case (#empty) {
               stack := stack2;
               next()
             };
        case (#leaf({keyvals=null})) {
               stack := stack2;
               next()
             };
        case (#leaf({count=c; keyvals=?((k,v),kvs)})) {
               stack := ?(#leaf({count=c-1; keyvals=kvs}), stack2);
               ?(k.key, v)
             };
        case (#branch(br)) {
               stack := ?(br.left, ?(br.right, stack2));
               next()
             };
          }
         }
        }
      }
    };

  };


// clone cannot be an efficient object method,
// ...but is still useful in tests, and beyond.
public func clone<K,V>
  (h:TrieMap<K,V>,
   keyEq: (K,K) -> Bool,
   keyHash: K -> Hash.Hash) : TrieMap<K,V> {
  let h2 = TrieMap<K,V>(keyEq, keyHash);
  for ((k,v) in h.iter()) {
    ignore h2.set(k,v);
  };
  h2
};

// Clone from any iterator of key-value pairs
public func fromIter<K, V>(iter:I.Iter<(K, V)>,
                           keyEq: (K,K) -> Bool,
                           keyHash: K -> Hash.Hash) : TrieMap<K,V> {
  let h = TrieMap<K,V>(keyEq, keyHash);
  for ((k,v) in iter) {
    ignore h.set(k,v);
  };
  h
};

public func map<K, V1, V2>
  (h:TrieMap<K,V1>,
   keyEq: (K,K) -> Bool,
   keyHash: K -> Hash.Hash,
   mapFn: (K, V1) -> V2,
  ) : TrieMap<K,V2> {
  let h2 = TrieMap<K,V2>(keyEq, keyHash);
  for ((k, v1) in h.iter()) {
    let v2 = mapFn(k, v1);
    ignore h2.set(k,v2);
  };
  h2
};

public func mapFilter<K, V1, V2>
  (h:TrieMap<K,V1>,
   keyEq: (K,K) -> Bool,
   keyHash: K -> Hash.Hash,
   mapFn: (K, V1) -> ?V2,
  ) : TrieMap<K,V2> {
  let h2 = TrieMap<K,V2>(keyEq, keyHash);
  for ((k, v1) in h.iter()) {
    switch (mapFn(k, v1)) {
      case null { };
      case (?v2) {
             ignore h2.set(k,v2);
           };
    }
  };
  h2
};

}
