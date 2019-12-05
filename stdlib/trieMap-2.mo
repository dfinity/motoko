import T "trie.mo";
import P "prelude.mo";

// Task: Implement `insert`.
module {
public class Map<K,V> (isEq:(K, K) -> Bool, hashOf: K -> T.Hash) {
  var map : T.Trie<K,V> = T.empty<K,V>();
  // update map mutably; return old element, if any
  public func insert(k:K, v:V) : ?V {
    let keyObj = {key=k; hash=hashOf(k);};
    let (map2, ov) = T.insert<K,V>(map, keyObj, isEq, v);
    map := map2;
    ov
  };
};
}
