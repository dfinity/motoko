/*
 * Hash Tries.
 *
 *  Functional maps (and sets) whose representation is "canonical", and history independent.
 *
 *  By contrast, AVL Trees, RB Trees, and other representations do not
 *  enjoy history independence, and are each more complex to implement
 *  (e.g., each requires "rebalancing"; these trees never do).
 *
 * See this POPL 1989 paper (Section 6):
 *   - "Incremental computation via function caching", Pugh & Teitelbaum.
 *   - https://dl.acm.org/citation.cfm?id=75305
 *   - Public copy here: http://matthewhammer.org/courses/csci7000-s17/readings/Pugh89.pdf
 */

// Done:
//
//  - (hacky) type definition; XXX: need real sum types to clean it up
//  - find operation
//  - insert operation
//  - remove operation
//  - replace operation (remove+insert via a single traversal)

// TODO-Matthew:
//
//  - regression tests for everything that is below
//
//  - handle hash collisions gracefully
//
//  - iterator objects, for use in 'for ... in ...' patterns

// TODO: Replace this definition WordX, for some X, once we have these types in AS.
type Hash = ?(Bool, Hash);
//type Hash = Word16;

// XXX: This Node type is a "sloppy union" between "BinNodes" (left/right fields) and "Leaves" (key/val fields):
type Node<K,V> = {left:Trie<K,V>; right:Trie<K,V>; key:?K; val:?V};
type Trie<K,V> = ?Node<K,V>;

// Simplifying assumption, for now: All defined paths in the trie have a uniform length,
// the same as the number of bits of a hash, starting at the LSB, that we use for indexing.
//
// - If the number is too low, our expected O(log n) bounds become expected O(n).
// - If the number is too high, we waste constant factors for representing small sets/maps.
//
// TODO: Make this more robust by making this number adaptive for each
// path, and based on the content of the trie along that path.
//
let HASH_BITS = 4;

// XXX: Until we have real sum types:
func assertIsNull<X>(x : ?X) {
  switch x {
    case null { assert(true)  };
    case (?_) { assert(false) };
  };
};

// XXX: Until we have real sum types:
func assertIsBin<K,V>(t : Trie<K,V>) {
  switch t {
    case null { assert(false) };
    case (?n) {
      assertIsNull<K>(n.key);
      assertIsNull<V>(n.val);
   };
  }
};

// XXX: this helper is an ugly hack; we need real sum types to avoid it, I think:
func getLeafKey<K,V>(t : Node<K,V>) : K {
  assertIsNull<Node<K,V>>(t.left);
  assertIsNull<Node<K,V>>(t.right);
  switch (t.key) {
    case (?k) { k };
    case null { getLeafKey<K,V>(t) };
  }
};

// XXX: this helper is an ugly hack; we need real sum types to avoid it, I think:
func getLeafVal<K,V>(t : Node<K,V>) : ?V {
  assertIsNull<Node<K,V>>(t.left);
  assertIsNull<Node<K,V>>(t.right);
  t.val
};

// TODO: Replace with bitwise operations on Words, once we have each of those in AS.  For now, we encode hashes as lists of booleans.
func getHashBit(h:Hash, pos:Nat) : Bool {
  switch h {
  case null { false }; // TODO: Should be an error case
  case (?(b, _)) {
         if (pos == 0) { b }
         else { getHashBit(h, pos-1) }
       };
  }
};

func empty<K,V>() : Trie<K,V> =
  null
;

// helper function for constructing new paths of uniform length
func buildNewPath<K,V>(bitpos:Nat, k:K, k_hash:Hash, ov:?V) : Trie<K,V> {
  func rec(bitpos:Nat) : Trie<K,V> {
    if ( bitpos < HASH_BITS ) {
      // create new bin node for this bit of the hash
      let path = rec(bitpos+1);
      let bit = getHashBit(k_hash, bitpos);
      if bit { ?(new {left=path; right=null; key=null; val=null}) }
      else   { ?(new {left=null; right=path; key=null; val=null}) }
    } else {
      // create new leaf for (k,v) pair
      ?(new {left=null; right=null; key=?k; val=ov })
    }
  };
  rec(bitpos)
};

// replace the given key's value option with the given one, returning the previous one
func replace<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, v:?V) : (Trie<K,V>, ?V) {
  // For `bitpos` in 0..HASH_BITS, walk the given trie and locate the given value `x`, if it exists.
  func rec(t : Trie<K,V>, bitpos:Nat) : (Trie<K,V>, ?V) {
    if ( bitpos < HASH_BITS ) {
      assertIsBin<K,V>(t);
      switch t {
      case null { (buildNewPath<K,V>(bitpos, k, k_hash, v), null) };
      case (?n) {
        let bit = getHashBit(k_hash, bitpos);
        // rebuild either the left or right path with the inserted (k,v) pair
        if bit {
          let (l, v_) = rec(n.left, bitpos+1);
          (?(new {left=l; right=n.right; key=null; val=null }), v_)
        }
        else {
          let (r, v_) = rec(n.right, bitpos+1);
          (?(new {left=n.left; right=r; key=null; val=null }), v_)
        }
      };
    }
    } else {
      // No more walking; we should be at a leaf now, by construction invariants.
      switch t {
        case null { (buildNewPath<K,V>(bitpos, k, k_hash, v), null) };
        case (?l) {
           // TODO: Permit hash collisions by walking a list/array of KV pairs in each leaf:
           (?(new{left=null;right=null;key=?k;val=v}), l.val)
        };
      }
    }
  };
  rec(t, 0)
};

// insert the given key's value in the trie; return the new trie
func insert<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, v:V) : (Trie<K,V>, ?V) {
  replace<K,V>(t, k, k_hash, ?v)
};

// remove the given key's value in the trie; return the new trie
func remove<K,V>(t : Trie<K,V>, k:K, k_hash:Hash) : (Trie<K,V>, ?V) {
  replace<K,V>(t, k, k_hash, null)
};

// find the given key's value in the trie, or return null if nonexistent
func find<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, keq:(K,K) -> Bool) : ?V {
  // For `bitpos` in 0..HASH_BITS, walk the given trie and locate the given value `x`, if it exists.
  func rec(t : Trie<K,V>, bitpos:Nat) : ?V {
    if ( bitpos < HASH_BITS ) {
      assertIsBin<K,V>(t);
      switch t {
      case null {
        // the trie may be "sparse" along paths leading to no keys, and may end early.
        null
      };
      case (?n) {
        let bit = getHashBit(k_hash, bitpos);
        if bit { rec(n.left,  bitpos+1) }
        else   { rec(n.right, bitpos+1) }
        };
      }
    } else {
      // No more walking; we should be at a leaf now, by construction invariants.
      switch t {
        case null { null };
        case (?l) {
           // TODO: Permit hash collisions by walking a list/array of KV pairs in each leaf:
           if (keq(getLeafKey<K,V>(l), k)) {
             getLeafVal<K,V>(l)
           } else {
             null
           }
        };
      }
    }
  };
  rec(t, 0)
};

///////////////////////////////////////////////////////////////////////

/*
 Sets are partial maps from element type to unit type,
 i.e., the partial map represents the set with its domain.
*/

// TODO-Matthew:
//
// - for now, we pass a hash value each time we pass an element value;
//   in the future, we might avoid passing element hashes with each element in the API;
//   related to: https://github.com/dfinity-lab/actorscript/issues/157
//

type Set<T> = Trie<T,()>;

func setEmpty<T>():Set<T> =
  empty<T,()>();

func setInsert<T>(s:Set<T>, x:T, xh:Hash):Set<T> = {
  let (s2, _) = insert<T,()>(s, x, xh, ());
  s2
};

func setRemove<T>(s:Set<T>, x:T, xh:Hash):Set<T> = {
  let (s2, _) = remove<T,()>(s, x, xh);
  s2
};

func setMem<T>(s:Set<T>, x:T, xh:Hash, eq:(T,T)->Bool):Bool {
  switch (find<T,()>(s, x, xh, eq)) {
  case null { false };
  case (?_) { true };
  }
};

func setUnion<T>(s1:Set<T>, s2:Set<T>):Set<T> { /* TODO */ setUnion<T>(s1,s2) };
func setDiff<T>(s1:Set<T>, s2:Set<T>):Set<T> { /* TODO */ setDiff<T>(s1,s2) };
func setIntersect<T>(s1:Set<T>, s2:Set<T>):Set<T> { /* TODO */ setIntersect<T>(s1,s2) };

// Insert numbers [1..8] into the set, using their bits as their hashes:
let s0 : Set<Nat> = setEmpty<Nat>();
let s1 : Set<Nat> = setInsert<Nat>(s0, 1, ?(false,?(false,?(false,?(false, null))))); // 0 0 0 0
let s2 : Set<Nat> = setInsert<Nat>(s1, 2, ?(true, ?(false,?(false,?(false, null))))); // 1 0 0 0
let s3 : Set<Nat> = setInsert<Nat>(s2, 3, ?(false,?(true, ?(false,?(false, null))))); // 0 1 0 0
let s4 : Set<Nat> = setInsert<Nat>(s3, 4, ?(true, ?(true, ?(false,?(false, null))))); // 1 1 0 0
let s5 : Set<Nat> = setInsert<Nat>(s4, 5, ?(false,?(false,?(true, ?(false, null))))); // 0 0 1 0
let s6 : Set<Nat> = setInsert<Nat>(s5, 6, ?(true, ?(false,?(true, ?(false, null))))); // 1 0 1 0
let s7 : Set<Nat> = setInsert<Nat>(s6, 7, ?(true, ?(true, ?(true, ?(false, null))))); // 1 1 1 0
let s8 : Set<Nat> = setInsert<Nat>(s7, 8, ?(false,?(false,?(false,?(true,  null))))); // 1 1 1 0
