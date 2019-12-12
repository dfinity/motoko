// rename 'trie' into 'funMap'? -- makes sense to me
import Trie "trie.mo";

module {
  public type Map<X, Y> = Trie.Trie<X, Y>;

}
