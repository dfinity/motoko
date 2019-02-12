/*
 Hash Tries in ActorScript
 -------------------------

 Functional maps (and sets) whose representation is "canonical", and
 history independent.

  See this POPL 1989 paper (Section 6):
    - "Incremental computation via function caching", Pugh & Teitelbaum.
    - https://dl.acm.org/citation.cfm?id=75305
    - Public copy here: http://matthewhammer.org/courses/csci7000-s17/readings/Pugh89.pdf

 By contrast, other usual functional representations of maps (AVL
 Trees, Red-Black Trees) do not enjoy history independence, and are
 each more complex to implement (e.g., each requires "rebalancing";
 these trees never do).

 */

// Done:
//
//  - (hacky) type definition; XXX: need real sum types to clean it up
//  - find operation
//  - insert operation
//  - remove operation
//  - replace operation (remove+insert via a single traversal)
//  - basic encoding of sets, and some set operations
//  - basic tests (and primitive debugging) for set operations

// TODO-Matthew:
//
//  - write trie operations that operate over pairs of tries:
//    for set union, difference and intersection.
//
//  - (more) regression tests for everything that is below
//
//  - handle hash collisions gracefully
//
//  - iterator objects, for use in 'for ... in ...' patterns

// TEMP: A "bit string" as a linked list of bits:
type Bits = ?(Bool, Bits);

// TODO: Replace this definition WordX, for some X, once we have these types in AS.
type Hash = Bits;
//type Hash = Word16;

// XXX: This Node type is a "sloppy union" between "BinNodes" (left/right fields) and "Leaves" (key/val fields):
type Node<K,V> = {
  left:Trie<K,V>;
  right:Trie<K,V>;
  key:?K;
  val:?V
};

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

// TODO: Replace with bitwise operations on Words, once we have each of those in AS.
// For now, we encode hashes as lists of booleans.
func getHashBit(h:Hash, pos:Nat) : Bool {
  switch h {
  case null {
         // XXX: Should be an error case; it shouldn't happen in our tests if we set them up right.
         false
       };
  case (?(b, h_)) {
         if (pos == 0) { b }
         else { getHashBit(h_, pos-1) }
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
      if (not bit) {
        ?(new {left=path; right=null; key=null; val=null})
      }
      else {
        ?(new {left=null; right=path; key=null; val=null})
      }
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
      switch t {
      case null { (buildNewPath<K,V>(bitpos, k, k_hash, v), null) };
      case (?n) {
        assertIsBin<K,V>(t);
        let bit = getHashBit(k_hash, bitpos);
        // rebuild either the left or right path with the inserted (k,v) pair
        if (not bit) {
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
      switch t {
      case null {
        // the trie may be "sparse" along paths leading to no keys, and may end early.
        null
      };
      case (?n) {
        assertIsBin<K,V>(t);
        let bit = getHashBit(k_hash, bitpos);
        if (not bit) { rec(n.left,  bitpos+1) }
        else         { rec(n.right, bitpos+1) }
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

////////////////////////////////////////////////////////////////////

func setPrint(s:Set<Nat>) {
  func rec(s:Set<Nat>, ind:Nat, bits:Hash) {
    func indPrint(i:Nat) {
      if (i == 0) { } else { print "| "; indPrint(i-1) }
    };
    func bitsPrintRev(bits:Bits) {
      switch bits {
        case null { print "" };
        case (?(bit,bits_)) {
               bitsPrintRev(bits_);
               if bit { print "1R." }
               else   { print "0L." }
             }
      }
    };
    switch s {
    case null {
           //indPrint(ind);
           //bitsPrintRev(bits);
           //print "(null)\n";
         };
    case (?n) {
           switch (n.key) {
           case null {
                  //indPrint(ind);
                  //bitsPrintRev(bits);
                  //print "bin \n";
                  rec(n.right, ind+1, ?(true, bits));
                  rec(n.left,  ind+1, ?(false,bits));
                  //bitsPrintRev(bits);
                  //print ")\n"
                };
           case (?k) {
                  //indPrint(ind);
                  bitsPrintRev(bits);
                  print "(leaf ";
                  printInt k;
                  print ")\n";
                };
           }
         };
    }
  };
  rec(s, 0, null);
};

////////////////////////////////////////////////////////////////////////////////

func setInsertDb(s:Set<Nat>, x:Nat, xh:Hash):Set<Nat> = {
  print "  setInsert(";
  printInt x;
  print ")";
  let r = setInsert<Nat>(s,x,xh);
  print ";\n";
  setPrint(r);
  r
};

func setMemDb(s:Set<Nat>, sname:Text, x:Nat, xh:Hash):Bool = {
  func natEq(n:Nat,m:Nat):Bool{ n == m};
  print "  setMem(";
  print sname;
  print ", ";
  printInt x;
  print ")";
  let b = setMem<Nat>(s,x,xh,natEq);
  if b { print " = true" } else { print " = false" };
  print ";\n";
  b
};

/////////////////////////////////////////////////////////////////////////////////

let hash_0 = ?(false,?(false,?(false,?(false, null))));
let hash_1 = ?(false,?(false,?(false,?(true,  null))));
let hash_2 = ?(false,?(false,?(true, ?(false, null))));
let hash_3 = ?(false,?(false,?(true, ?(true,  null))));
let hash_4 = ?(false,?(true, ?(false,?(false, null))));
let hash_5 = ?(false,?(true, ?(true, ?(true,  null))));
let hash_6 = ?(false,?(true, ?(true, ?(false, null))));
let hash_7 = ?(false,?(true, ?(true, ?(true,  null))));
let hash_8 = ?(true, ?(false,?(false,?(false, null))));

print "inserting...\n";
// Insert numbers [0..8] into the set, using their bits as their hashes:
let s0 : Set<Nat> = setEmpty<Nat>();
let s1 : Set<Nat> = setInsertDb(s0, 0, hash_0);
let s2 : Set<Nat> = setInsertDb(s1, 1, hash_1);
let s3 : Set<Nat> = setInsertDb(s2, 2, hash_2);
let s4 : Set<Nat> = setInsertDb(s3, 3, hash_3);
let s5 : Set<Nat> = setInsertDb(s4, 4, hash_4);
let s6 : Set<Nat> = setInsertDb(s5, 5, hash_5);
let s7 : Set<Nat> = setInsertDb(s6, 6, hash_6);
let s8 : Set<Nat> = setInsertDb(s7, 7, hash_7);
let s9 : Set<Nat> = setInsertDb(s8, 8, hash_8);
print "done.\n";
print "testing membership...\n";

// Element 0: Test memberships of each set defined above for element 0
assert( not( setMemDb(s0, "s0", 0, hash_0 ) ));
assert( setMemDb(s1, "s1", 0, hash_0 ) );
assert( setMemDb(s2, "s2", 0, hash_0 ) );
assert( setMemDb(s3, "s3", 0, hash_0 ) );
assert( setMemDb(s4, "s4", 0, hash_0 ) );
assert( setMemDb(s5, "s5", 0, hash_0 ) );
assert( setMemDb(s6, "s6", 0, hash_0 ) );
assert( setMemDb(s7, "s7", 0, hash_0 ) );
assert( setMemDb(s8, "s8", 0, hash_0 ) );

// Element 1: Test memberships of each set defined above for element 1
assert( not(setMemDb(s0, "s0", 1, hash_1 )) );
assert( not(setMemDb(s1, "s1", 1, hash_1 )) );
assert( setMemDb(s2, "s2", 1, hash_1 ) );
assert( setMemDb(s3, "s3", 1, hash_1 ) );
assert( setMemDb(s4, "s4", 1, hash_1 ) );
assert( setMemDb(s5, "s5", 1, hash_1 ) );
assert( setMemDb(s6, "s6", 1, hash_1 ) );
assert( setMemDb(s7, "s7", 1, hash_1 ) );
assert( setMemDb(s8, "s8", 1, hash_1 ) );

// Element 2: Test memberships of each set defined above for element 2
assert( not(setMemDb(s0, "s0", 2, hash_2 )) );
assert( not(setMemDb(s1, "s1", 2, hash_2 )) );
assert( not(setMemDb(s2, "s2", 2, hash_2 )) );
assert( setMemDb(s3, "s3", 2, hash_2 ) );
assert( setMemDb(s4, "s4", 2, hash_2 ) );
assert( setMemDb(s5, "s5", 2, hash_2 ) );
assert( setMemDb(s6, "s6", 2, hash_2 ) );
assert( setMemDb(s7, "s7", 2, hash_2 ) );
assert( setMemDb(s8, "s8", 2, hash_2 ) );

// Element 3: Test memberships of each set defined above for element 3
assert( not(setMemDb(s0, "s0", 3, hash_3 )) );
assert( not(setMemDb(s1, "s1", 3, hash_3 )) );
assert( not(setMemDb(s2, "s2", 3, hash_3 )) );
assert( not(setMemDb(s3, "s3", 3, hash_3 )) );
assert( setMemDb(s4, "s4", 3, hash_3 ) );
assert( setMemDb(s5, "s5", 3, hash_3 ) );
assert( setMemDb(s6, "s6", 3, hash_3 ) );
assert( setMemDb(s7, "s7", 3, hash_3 ) );
assert( setMemDb(s8, "s8", 3, hash_3 ) );

print "done.\n";
