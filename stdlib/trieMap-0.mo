import T "trie.mo";
import P "prelude.mo";

module {

/**

Summary: Maps as objects, whose private representation uses a single
         mutable variable holding an immutable trie.

Why? 
-----

The Trie module implements immutable hash maps, which can have
algorithmic benefits sometimes.

However, the current Trie module uses a low-level, functional
interface.

Instead, sometimes we want an imperative OO interface, as in certain
cases in the produce exchange (where this OO level would be nice in
places, but is missing).

This module defines that OO Map interface, and implements it with the Trie module.

*/

public class Map<K,V> (isEq:(K, K) -> Bool, hashOf: K -> T.Hash) {
  
  var map : T.Trie<K,V> = #empty;

  // update map mutably; return old element, if any
  public func insert(k:K) : ?V {
    P.xxx()
  };
};
}
