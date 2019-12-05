import T "trie.mo";
import P "prelude.mo";

module {

public class Map<K,V> (isEq:(K, K) -> Bool, hashOf: K -> T.Hash) {

  // won't actually work!
  var map : T.Trie<K,V> = #empty;

  // we will need this instead:
  // var map : T.Trie<K,V> = T.empty<K,V>();

  // update map mutably; return old element, if any
  public func insert(k:K) : ?V {
    P.xxx()
  };
};
}
