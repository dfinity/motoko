/**

Hash tries
======================

Functional maps (and sets) whose representation is "canonical", and
history independent.

Background
------------------

See this POPL 1989 paper (Section 6):

 - ["Incremental computation via function caching", Pugh & Teitelbaum](https://dl.acm.org/citation.cfm?id=75305).
 - [Public copy here](http://matthewhammer.org/courses/csci7000-s17/readings/Pugh89.pdf).

By contrast, other usual functional representations of maps (AVL
Trees, Red-Black Trees) do not enjoy history independence, and are
each more complex to implement (e.g., each requires "rebalancing";
these trees never do).

Assumptions
=============

Uniform depth assumption:
------------------------------

We make a simplifying assumption, for now: All defined paths in the
trie have a uniform length, the same as the number of bits of a
hash, starting at the LSB, that we use for indexing.

- If the number is too low, our expected O(log n) bounds become
  expected O(n).

- If the number is too high, we waste constant factors for
  representing small sets/maps.

In [future work](#adaptive-path-lengths), we can make this more robust
by making this number adaptive for each path, and based on the content
of the trie along that path.

See [Future work](#future-work) below
--------------------------------------
*/

/**
Representation
=====================

Below, we define the types used in the representation:

 - **`Key<K>`**, parameterized by a hashable type `K`
 - **`Branch<K,V>`**, for binary nodes with two tries.
 - **`Leaf<K,V>`**, for leaf nodes with no sub-tries.  We use [association lists](#association-lists) there.
 - We use **`null`** for the empty trie, and
 - **`?Node<K,V>`** represents all three possibilities until we have variants.


See the full details in the definitions below:

*/

let Hash = (import "hash.as").BitVec;
type Hash = Hash.t;

let List = import "List.as";
type List<T> = List.List<T>;

let AssocList = import "assocList.as";
type AssocList<K,V> = AssocList.AssocList<K,V>;

let HASH_BITS = 4;

type Key<K> = {
  // hash field: permits fast inequality checks, permits collisions;
  // (eventually: permits incremental growth of deeper trie paths)
  hash: Hash;
  // key field: for conservative equality checks, after equal hashes.
  key: K;
};

// Binary branch nodes
type Branch<K,V> = {
  left:Trie<K,V>;
  right:Trie<K,V>;
};
// Leaf nodes are association lists of `Key<K>`s where every key
// shares a common hash prefix, its (common) trie path.
type Leaf<K,V> = {
  keyvals:AssocList<Key<K>,V>;
};

// XXX: See AST-42
type Node<K,V> = {
  left:Trie<K,V>;
  right:Trie<K,V>;
  keyvals:AssocList<Key<K>,V>;
};

type Trie<K,V> = ?Node<K,V>;

/**
 Association lists
 -----------------
 Notice above that the `Leaf` case uses a list of key-value pairs.

 See [this document]($DOCURL/assocList.html) for more details.
*/

/**
 Two-dimensional trie
 ---------------------
 A 2D trie is just a trie that maps dimension-1 keys to another
 layer of tries, each keyed on the dimension-2 keys.
*/
type Trie2D<K1, K2, V> = Trie<K1, Trie<K2,V> >;

/**
 Three-dimensional trie
 ---------------------
 A 3D trie is just a trie that maps dimension-1 keys to another
 layer of 2D tries, each keyed on the dimension-2 and dimension-3 keys.
*/
type Trie3D<K1, K2, K3, V> = Trie<K1, Trie2D<K2, K3, V> >;

/**
 Module interface
 ===================

 For non-public helpers used in these definitions, see below:

 - [Helpers 1](#helpers-for-hashing)
 - [Helpers 2](#helpers-for-missing-variants)

 */

//let Trie = new {

  /**
   `empty`
   --------
   An empty trie.
   */
  func empty<K,V>() : Trie<K,V> = makeEmpty<K,V>();

  /**
   `copy`
   ---------
   Purely-functional representation permits _O(1)_-time copy, via persistent sharing.

   */

  func copy<K, V>(t : Trie<K, V>) : Trie<K, V> = t;

  /**
   `replace`
   ---------
   replace the given key's value option with the given one, returning the previous one
   */
  func replace<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool, v:?V) : (Trie<K,V>, ?V) {
    let key_eq = keyEq<K>(k_eq);
    // For `bitpos` in 0..HASH_BITS, walk the given trie and locate the given value `x`, if it exists.
    func rec(t : Trie<K,V>, bitpos:Nat) : (Trie<K,V>, ?V) {
      if ( bitpos < HASH_BITS ) {
	      switch t {
	      case null { (buildNewPath<K,V>(bitpos, k, v), null) };
	      case (?n) {
	             assertIsBin<K,V>(t);
	             let bit = Hash.getHashBit(k.hash, bitpos);
	             // rebuild either the left or right path with the inserted (k,v) pair
	             if (not bit) {
	               let (l, v_) = rec(n.left, bitpos+1);
	               (?(new {left=l; right=n.right; keyvals=null; }), v_)
	             }
	             else {
	               let (r, v_) = rec(n.right, bitpos+1);
	               (?(new {left=n.left; right=r; keyvals=null; }), v_)
	             }
	           };
        }
      } else {
	      // No more walking; we should be at a leaf now, by construction invariants.
	      switch t {
	      case null { (buildNewPath<K,V>(bitpos, k, v), null) };
	      case (?l) {
	             // Permit hash collisions by walking
               // a list/array of KV pairs in each leaf:
               let (kvs2, old_val) =
                 AssocList.replace<Key<K>,V>(l.keyvals, k, key_eq, v);
	             (?(new{left=null; right=null; keyvals=kvs2}), old_val)
	           };
	      }
      }
    };
    rec(t, 0)
  };

  /**
   `replaceThen`
   ------------
   replace the given key's value in the trie,
   and only if successful, do the success continuation,
   otherwise, return the failure value
   */
  func replaceThen<K,V,X>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool, v2:V,
                         success: (Trie<K,V>, V) -> X,
                         fail: () -> X)
    : X
  {
    let (t2, ov) = replace<K,V>(t, k, k_eq, ?v2);
    switch ov {
      case (null) { /* no prior value; failure to remove */ fail() };
      case (?v1) { success(t2, v1) };
    }
  };

  /**
   `insert`
   ------------
   insert the given key's value in the trie; return the new trie, and the previous value associated with the key, if any
   */
  func insert<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool, v:V) : (Trie<K,V>, ?V) {
    replace<K,V>(t, k, k_eq, ?v)
  };

  /**
   `insertFresh`
   ----------------
   insert the given key's value in the trie; return the new trie; assert that no prior value is associated with the key
   */
  func insertFresh<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool, v:V) : Trie<K,V> {
    let (t2, none) = replace<K,V>(t, k, k_eq, ?v);
    switch none {
      case (null) ();
      case (?_) assert false;
    };
    t2
  };

  /**
   `insert2D`
   ---------------
   insert the given key's value in the 2D trie; return the new 2D trie.
   */
  func insert2D<K1,K2,V>(t : Trie2D<K1,K2,V>,
                              k1:Key<K1>, k1_eq:(K1,K1)->Bool,
                              k2:Key<K2>, k2_eq:(K2,K2)->Bool,
                              v:V)
    : Trie2D<K1,K2,V>
  {
    let inner = find<K1,Trie<K2,V>>(t, k1, k1_eq);
    let (updated_inner, _) = switch inner {
    case (null)   { insert<K2,V>(null, k2, k2_eq, v) };
    case (?inner) { insert<K2,V>(inner, k2, k2_eq, v) };
    };
    let (updated_outer, _) = { insert<K1,Trie<K2,V>>(t, k1, k1_eq, updated_inner) };
    updated_outer;
  };

  /**
   `insert3D`
   ---------------
   insert the given key's value in the trie; return the new trie;
   */
  func insert3D<K1,K2,K3,V>
    (t : Trie3D<K1,K2,K3,V>,
     k1:Key<K1>, k1_eq:(K1,K1)->Bool,
     k2:Key<K2>, k2_eq:(K2,K2)->Bool,
     k3:Key<K3>, k3_eq:(K3,K3)->Bool,
     v:V
    )
    : Trie3D<K1,K2,K3,V>
  {
    let inner1 = find<K1,Trie2D<K2,K3,V>>(t, k1, k1_eq);
    let (updated_inner1, _) = switch inner1 {
    case (null)   {
           insert<K2,Trie<K3,V>>(
             null, k2, k2_eq,
             (insert<K3,V>(null, k3, k3_eq, v)).0
           )
         };
    case (?inner1) {
           let inner2 = find<K2,Trie<K3,V>>(inner1, k2, k2_eq);
           let (updated_inner2, _) = switch inner2 {
           case (null) { insert<K3,V>(null, k3, k3_eq, v) };
           case (?inner2) { insert<K3,V>(inner2, k3, k3_eq, v) };
           };
           insert<K2,Trie<K3,V>>( inner1, k2, k2_eq, updated_inner2 )
         };
    };
    let (updated_outer, _) = { insert<K1,Trie2D<K2,K3,V>>(t, k1, k1_eq, updated_inner1) };
    updated_outer;
  };

  /**
   `remove`
   -------------
   remove the given key's value in the trie; return the new trie
   */
  func remove<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool) : (Trie<K,V>, ?V) {
    replace<K,V>(t, k, k_eq, null)
  };

  /**
   `removeThen`
   ------------
   remove the given key's value in the trie,
   and only if successful, do the success continuation,
   otherwise, return the failure value
   */
  func removeThen<K,V,X>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K)->Bool,
                         success: (Trie<K,V>, V) -> X,
                         fail: () -> X)
    : X
  {
    let (t2, ov) = replace<K,V>(t, k, k_eq, null);
    switch ov {
      case (null) { /* no prior value; failure to remove */ fail() };
      case (?v) { success(t2, v) };
    }
  };


  /**
   `remove2D`
   --------------
   remove the given key-key pair's value in the 2D trie; return the
   new trie, and the prior value, if any.
   */
  func remove2D<K1,K2,V>(t : Trie2D<K1,K2,V>,
                         k1:Key<K1>, k1_eq:(K1,K1)->Bool,
                         k2:Key<K2>, k2_eq:(K2,K2)->Bool)
    : (Trie2D<K1,K2,V>, ?V)
  {
    switch (find<K1,Trie<K2,V>>(t, k1, k1_eq)) {
    case (null)   {
           (t, null)
         };
    case (?inner) {
           let (updated_inner, ov) = remove<K2,V>(inner, k2, k2_eq);
           let (updated_outer, _) = {
             insert<K1,Trie<K2,V>>(t, k1, k1_eq, updated_inner)
           };
           (updated_outer, ov)
         };
    }
  };

  /**
   `remove3D`
   ---------------
   remove the given key-key pair's value in the 3D trie; return the
   new trie, and the prior value, if any.
   */
  func remove3D<K1,K2,K3,V>
    (t : Trie3D<K1,K2,K3,V>,
     k1:Key<K1>, k1_eq:(K1,K1)->Bool,
     k2:Key<K2>, k2_eq:(K2,K2)->Bool,
     k3:Key<K3>, k3_eq:(K3,K3)->Bool,
    )
    : (Trie3D<K1,K2,K3,V>, ?V)
  {
    switch (find<K1,Trie2D<K2,K3,V>>(t, k1, k1_eq)) {
    case (null)   {
           (t, null)
         };
    case (?inner) {
           let (updated_inner, ov) = remove2D<K2,K3,V>(inner, k2, k2_eq, k3, k3_eq);
           let (updated_outer, _) = {
             insert<K1,Trie2D<K2,K3,V>>(t, k1, k1_eq, updated_inner)
           };
           (updated_outer, ov)
         };
    }
  };


  /**
   `find`
   ---------
   find the given key's value in the trie, or return null if nonexistent
   */
  func find<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K) -> Bool) : ?V {
    let key_eq = keyEq<K>(k_eq);
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
	             let bit = Hash.getHashBit(k.hash, bitpos);
	             if (not bit) { rec(n.left,  bitpos+1) }
	             else         { rec(n.right, bitpos+1) }
	           };
	      }
      } else {
	      // No more walking; we should be at a leaf now, by construction invariants.
	      switch t {
	      case null { null };
	      case (?l) {
	             // Permit hash collisions by walking a list/array of KV pairs in each leaf:
               AssocList.find<Key<K>,V>(l.keyvals, k, key_eq)
	           };
	      }
      }
    };
    rec(t, 0)
  };

  /**
   `merge`
   ---------
   merge tries, preferring the right trie where there are collisions
   in common keys. note: the `disj` operation generalizes this `merge`
   operation in various ways, and does not (in general) loose
   information; this operation is a simpler, special case.

   See also:

   - [`disj`](#disj)
   - [`join`](#join)
   - [`prod`](#prod)

   */
  func merge<K,V>(tl:Trie<K,V>, tr:Trie<K,V>, k_eq:(K,K)->Bool): Trie<K,V> {
    let key_eq = keyEq<K>(k_eq);
    func rec(tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      switch (tl, tr) {
      case (null, _) { return tr };
      case (_, null) { return tl };
      case (?nl,?nr) {
             switch (isBin<K,V>(tl),
	                   isBin<K,V>(tr)) {
             case (true, true) {
	                  let t0 = rec(nl.left, nr.left);
	                  let t1 = rec(nl.right, nr.right);
	                  makeBin<K,V>(t0, t1)
	                };
             case (false, true) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  tr
	                };
             case (true, false) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  tr
	                };
             case (false, false) {
	                  /// handle hash collisions by using the association list:
	                  makeLeaf<K,V>(
                      AssocList.disj<Key<K>,V,V,V>(
                        nl.keyvals, nr.keyvals,
                        key_eq,
                        func (x:?V, y:?V):V = {
                          switch (x, y) {
                          case (null, null) {/* IMPOSSIBLE case: diverge. */ func x():V=x(); x()};
                          case (null, ?v) v;
                          case (?v, _) v;
                          }}
                      ))
	                };
	           }
           };
      }
    };
    rec(tl, tr)
  };

  /**
   `mergeDisjoint`
   ----------------
   like `merge`, it merges tries, but unlike `merge`, it signals a
   dynamic error if there are collisions in common keys between the
   left and right inputs.
   */
  func mergeDisjoint<K,V>(tl:Trie<K,V>, tr:Trie<K,V>, k_eq:(K,K)->Bool): Trie<K,V> {
    let key_eq = keyEq<K>(k_eq);
    func rec(tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      switch (tl, tr) {
      case (null, _) { return tr };
      case (_, null) { return tl };
      case (?nl,?nr) {
             switch (isBin<K,V>(tl),
	                   isBin<K,V>(tr)) {
             case (true, true) {
	                  let t0 = rec(nl.left, nr.left);
	                  let t1 = rec(nl.right, nr.right);
	                  makeBin<K,V>(t0, t1)
	                };
             case (false, true) {
	                  assert(false);
	                  // XXX impossible, until we lift uniform depth assumption
	                  tr
	                };
             case (true, false) {
	                  assert(false);
	                  // XXX impossible, until we lift uniform depth assumption
	                  tr
	                };
             case (false, false) {
	                  /// handle hash collisions by using the association list:
	                  makeLeaf<K,V>(
                      AssocList.disj<Key<K>,V,V,V>(
                        nl.keyvals, nr.keyvals,
                        key_eq,
                        func (x:?V, y:?V):V = {
                          switch (x, y) {
                          case (null, null) {
                                 /* IMPOSSIBLE case. */
                                 assert false; func x():V=x(); x()
                               };
                          case (?_, ?_) {
                                 /* INVALID case: left and right defined for the same key */
                                 assert false; func x():V=x(); x()
                               };
                          case (null, ?v) v;
                          case (?v, null) v;
                          }}
                      ))
	                };
	           }
           };
      }
    };
    rec(tl, tr)
  };


  /**
   `mergeDisjoint2D`
   --------------

   Like [`mergeDisjoint`](#mergedisjoint), except instead of merging a
   pair, it merges the collection of dimension-2 sub-trees of a 2D
   trie.

   */
  func mergeDisjoint2D<K1,K2,V>(t : Trie2D<K1,K2,V>, k1_eq:(K1,K1)->Bool, k2_eq:(K2,K2)->Bool)
    : Trie<K2,V>
  {
    foldUp<K1,Trie<K2,V>, Trie<K2,V>>
    ( t,
      func (t1:Trie<K2,V>, t2:Trie<K2,V>):Trie<K2,V> {  mergeDisjoint<K2,V>(t1, t2, k2_eq) },
      func (_:K1, t:Trie<K2,V>): Trie<K2,V> { t },
      null )
  };

  /**
   `diff`
   ------
   The key-value pairs of the final trie consists of those pairs of
   the left trie whose keys are not present in the right trie; the
   values of the right trie are irrelevant.
   */
  func diff<K,V,W>(tl:Trie<K,V>, tr:Trie<K,W>, k_eq:(K,K)->Bool) : Trie<K,V> {
    let key_eq = keyEq<K>(k_eq);
    func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,V> {
      switch (tl, tr) {
      case (null, _) { return makeEmpty<K,V>() };
      case (_, null) { return tl };
      case (?nl,?nr) {
             switch (isBin<K,V>(tl),
	                   isBin<K,W>(tr)) {
             case (true, true) {
	                  let t0 = rec(nl.left, nr.left);
	                  let t1 = rec(nl.right, nr.right);
	                  makeBin<K,V>(t0, t1)
	                };
             case (false, true) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  tl
	                };
             case (true, false) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  tl
	                };
             case (false, false) {
                    assert(isLeaf<K,V>(tl));
	                  assert(isLeaf<K,W>(tr));
                    makeLeaf<K,V>(
                      AssocList.diff<Key<K>,V,W>(nl.keyvals, nr.keyvals, key_eq)
                    )
	                };
	           }
           };
      }};
    rec(tl, tr)
  };

  /**
   `disj`
   --------

   This operation generalizes the notion of "set union" to finite maps.

   Produces a "disjunctive image" of the two tries, where the values of
   matching keys are combined with the given binary operator.

   For unmatched key-value pairs, the operator is still applied to
   create the value in the image.  To accomodate these various
   situations, the operator accepts optional values, but is never
   applied to (null, null).

   Implements the database idea of an ["outer join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).

   See also:

   - [`join`](#join)
   - [`merge`](#merge)
   - [`prod`](#prod)

   */
  func disj<K,V,W,X>(tl:Trie<K,V>, tr:Trie<K,W>,
			               k_eq:(K,K)->Bool, vbin:(?V,?W)->X)
    : Trie<K,X>
  {
    let key_eq = keyEq<K>(k_eq);
    func recL(t:Trie<K,V>) : Trie<K,X> {
      switch t {
	    case (null) null;
	    case (? n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?_) { makeLeaf<K,X>(AssocList.disj<Key<K>,V,W,X>(n.keyvals, null, key_eq, vbin)) };
	           case _ { makeBin<K,X>(recL(n.left), recL(n.right)) }
	           }
           };
      }};
    func recR(t:Trie<K,W>) : Trie<K,X> {
      switch t {
	    case (null) null;
	    case (? n) {
	           switch (matchLeaf<K,W>(t)) {
	           case (?_) { makeLeaf<K,X>(AssocList.disj<Key<K>,V,W,X>(null, n.keyvals, key_eq, vbin)) };
	           case _ { makeBin<K,X>(recR(n.left), recR(n.right)) }
	           }
           };
      }};
    func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      switch (tl, tr) {
        // empty-empty terminates early, all other cases do not.
      case (null, null) { makeEmpty<K,X>() };
      case (null, _   ) { recR(tr) };
      case (_,    null) { recL(tl) };
      case (? nl, ? nr) {
             switch (isBin<K,V>(tl),
	                   isBin<K,W>(tr)) {
             case (true, true) {
	                  let t0 = rec(nl.left, nr.left);
	                  let t1 = rec(nl.right, nr.right);
	                  makeBin<K,X>(t0, t1)
	                };
             case (false, true) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  makeEmpty<K,X>()
	                };
             case (true, false) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  makeEmpty<K,X>()
	                };
             case (false, false) {
	                  assert(isLeaf<K,V>(tl));
	                  assert(isLeaf<K,W>(tr));
                    makeLeaf<K,X>(
                      AssocList.disj<Key<K>,V,W,X>(nl.keyvals, nr.keyvals, key_eq, vbin)
                    )
                  };
	           }
           };
      }};
    rec(tl, tr)
  };

  /**
   `join`
   ---------
   This operation generalizes the notion of "set intersection" to
   finite maps.  Produces a "conjuctive image" of the two tries, where
   the values of matching keys are combined with the given binary
   operator, and unmatched key-value pairs are not present in the output.

   Implements the database idea of an ["inner join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).

   See also:

   - [`disj`](#disj)
   - [`merge`](#merge)
   - [`prod`](#prod)

   */
  func join<K,V,W,X>(tl:Trie<K,V>, tr:Trie<K,W>,
		                 k_eq:(K,K)->Bool, vbin:(V,W)->X)
    : Trie<K,X>
  {
    let key_eq = keyEq<K>(k_eq);
    func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      switch (tl, tr) {
	    case (null, null) { return makeEmpty<K,X>() };
	    case (null, ? nr) { return makeEmpty<K,X>() };
	    case (? nl, null) { return makeEmpty<K,X>() };
	    case (? nl, ? nr) {
	           switch (isBin<K,V>(tl),
		                 isBin<K,W>(tr)) {
	           case (true, true) {
	                  let t0 = rec(nl.left, nr.left);
	                  let t1 = rec(nl.right, nr.right);
	                  makeBin<K,X>(t0, t1)
	                };
	           case (false, true) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  makeEmpty<K,X>()
	                };
	           case (true, false) {
	                  assert false;
	                  // XXX impossible, until we lift uniform depth assumption
	                  makeEmpty<K,X>()
	                };
	           case (false, false) {
	                  assert(isLeaf<K,V>(tl));
	                  assert(isLeaf<K,W>(tr));
                    makeLeaf<K,X>(
                      AssocList.join<Key<K>,V,W,X>(nl.keyvals, nr.keyvals, key_eq, vbin)
                    )
	                };
	           }
	         }
      }};
    rec(tl, tr)
  };


  /**

   `prod`
   ---------

   Conditional _catesian product_, where the given
   operation `op` _conditionally_ creates output elements in the
   resulting trie.

   The keyed structure of the input tries are not relevant for this
   operation: all pairs are considered, regardless of keys matching or
   not.  Moreover, the resulting trie may use keys that are unrelated to
   these input keys.

   See also:

   - [`disj`](#disj)
   - [`join`](#join)
   - [`merge`](#merge)

   */
  func prod<K1,V1,K2,V2,K3,V3>(
    tl    :Trie<K1,V1>,
    tr    :Trie<K2,V2>,
    op    :(K1,V1,K2,V2) -> ?(Key<K3>,V3),
    k3_eq :(K3,K3) -> Bool
  )
    : Trie<K3,V3>
  {
    /**- binary case: merge disjoint results: */
    func merge (a:Trie<K3,V3>, b:Trie<K3,V3>) : Trie<K3,V3> =
      mergeDisjoint<K3,V3>(a, b, k3_eq);

    /**- `foldUp` "squared"; something like "`foldUp^2((tl, tr), merge, (insert null <?< op))`", but real: */
    foldUp<K1, V1, Trie<K3, V3>>(
      tl, merge,
      func (k1:K1, v1:V1) : Trie<K3,V3> {
        foldUp<K2, V2, Trie<K3, V3>>(
          tr, merge,
          func (k2:K2, v2:V2) : Trie<K3, V3> {
            switch (op(k1, v1, k2, v2)) {
              case null null;
              case (?(k3, v3)) { (insert<K3, V3>(null, k3, k3_eq, v3)).0 };
            }
          },
          null
        )
      },
      null
    )
  };

  /**
   `foldUp`
   ------------
   This operation gives a recursor for the internal structure of
   tries.  Many common operations are instantiations of this function,
   either as clients, or as hand-specialized versions (e.g., see map,
   mapFilter, exists and forAll below).
   */
  func foldUp<K,V,X>(t:Trie<K,V>, bin:(X,X)->X, leaf:(K,V)->X, empty:X) : X {
    func rec(t:Trie<K,V>) : X {
      switch t {
      case (null) { empty };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    AssocList.fold<Key<K>,V,X>(
                      kvs, empty,
                      func (k:Key<K>, v:V, x:X):X =
                        bin(leaf(k.key,v),x)
                    )
                  };
	           case null { bin(rec(n.left), rec(n.right)) };
	           }
           };
      }};
    rec(t)
  };

  /**
   `fold`
   ---------
   Fold over the key-value pairs of the trie, using an accumulator.
   The key-value pairs have no reliable or meaningful ordering.
   */
  func fold<K,V,X>(t:Trie<K,V>, f:(K,V,X)->X, x:X) : X {
    func rec(t:Trie<K,V>, x:X) : X {
      switch t {
      case (null) x;
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    AssocList.fold<Key<K>,V,X>(
                      kvs, x,
                      func (k:Key<K>, v:V, x:X):X = f(k.key,v,x)
                    )
                  };
	           case null { rec(n.left,rec(n.right,x)) };
	           }
           };
      }};
    rec(t, x)
  };

  /**
   `exists`
   --------
   Test whether a given key-value pair is present, or not.
   */
  func exists<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Bool {
    func rec(t:Trie<K,V>) : Bool {
      switch t {
      case (null) { false };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    List.exists<(Key<K>,V)>(
                      kvs, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
                    )
                  };
	           case null { rec(n.left) or rec(n.right) };
	           }
           };
      }};
    rec(t)
  };


  /**
   `forAll`
   ---------
   Test whether all key-value pairs have a given property.
   */
  func forAll<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Bool {
    func rec(t:Trie<K,V>) : Bool {
      switch t {
      case (null) { true };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    List.all<(Key<K>,V)>(
                      kvs, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
                    )
                  };
	           case null { rec(n.left) and rec(n.right) };
	           }
           };
      }};
    rec(t)
  };

  /**
   `count`
   --------
   Count the number of entries in the trie.
   */
  func count<K,V>(t:Trie<K,V>):Nat{
    foldUp<K,V,Nat>
    (t,
     func(n:Nat,m:Nat):Nat{n+m},
     func(_:K,_:V):Nat{1},
     0)
  };

  /**
   `toArray`
   --------
   Gather the collection of key-value pairs into an array.

   To do: make this more efficient, using a single array allocation.
   */
  func toArray<K,V,W>(t:Trie<K,V>,f:(K,V)->[W]):[W]{
    func arrayAppend(x:[W],y:[W]):[W] {
      Array_tabulate<W> (
        x.len() + y.len(),
        func (i:Nat) : W {
          if (i >= x.len()) { y[i - x.len()] }
          else { x[i] }
        }
      )
    };
    foldUp<K,V,[W]>
    (t,
     arrayAppend,
     func(k:K, v:V):[W]{f(k,v)},
     [])
  };

  /**
   `isEmpty`
   -----------
   specialized foldUp operation.
   Test for "deep emptiness": subtrees that have branching structure,
   but no leaves.  These can result from naive filtering operations;
   filter uses this function to avoid creating such subtrees.
   */
  func isEmpty<K,V>(t:Trie<K,V>) : Bool {
    func rec(t:Trie<K,V>) : Bool {
      switch t {
      case (null) { true };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) { List.isNil<(Key<K>,V)>(kvs) };
	           case null { rec(n.left) and rec(n.right) };
	           }
	         };
      }
    };
    rec(t)
  };

  /**
   `filter`
   -----------
   filter the key-value pairs by a given predicate.
   */
  func filter<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Trie<K,V> {
    func rec(t:Trie<K,V>) : Trie<K,V> {
      switch t {
      case (null) { null };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    makeLeaf<K,V>(
                      List.filter<(Key<K>,V)>(kvs, func ((k:Key<K>,v:V)):Bool = f(k.key,v))
                    )
		              };
	           case null {
		                let l = rec(n.left);
		                let r = rec(n.right);
		                switch (isEmpty<K,V>(l),
			                      isEmpty<K,V>(r)) {
		                case (true,  true)  null;
		                case (false, true)  r;
		                case (true,  false) l;
		                case (false, false) makeBin<K,V>(l, r);
		                }
		              };
	           }
	         };
      }
    };
    rec(t)
  };

  /**
   `mapFilter`
   -----------
   map and filter the key-value pairs by a given partial mapping function.
   */
  func mapFilter<K,V,W>(t:Trie<K,V>, f:(K,V)->?W) : Trie<K,W> {
    func rec(t:Trie<K,V>) : Trie<K,W> {
      switch t {
      case (null) { null };
      case (?n) {
	           switch (matchLeaf<K,V>(t)) {
	           case (?kvs) {
                    makeLeaf<K,W>(
                      List.mapFilter<(Key<K>,V),(Key<K>,W)>
                    (kvs,
                     // retain key and hash, but update key's value using f:
                     func ((k:Key<K>,v:V)):?(Key<K>,W) = {
                       switch (f(k.key,v)) {
                         case (null) null;
                         case (?w) (?(new {key=k.key; hash=k.hash}, w));
                       }}
                    ))
                  };
	           case null {
		                let l = rec(n.left);
		                let r = rec(n.right);
		                switch (isEmpty<K,W>(l),
			                      isEmpty<K,W>(r)) {
		                case (true,  true)  null;
		                case (false, true)  r;
		                case (true,  false) l;
		                case (false, false) makeBin<K,W>(l, r);
		                }
		              };
	           }
	         };
      }
    };
    rec(t)
  };

  /**
   `equalStructure`
   ------------------

   Test for equality, but naively, based on structure.
   Does not attempt to remove "junk" in the tree;
   For instance, a "smarter" approach would equate
     `#bin{left=#empty;right=#empty}`
   with
     `#empty`.
   We do not observe that equality here.
   */
  func equalStructure<K,V>(
    tl:Trie<K,V>,
    tr:Trie<K,V>,
    keq:(K,K)->Bool,
    veq:(V,V)->Bool
  ) : Bool {
    func rec(tl:Trie<K,V>, tr:Trie<K,V>) : Bool {
      switch (tl, tr) {
      case (null, null) { true };
      case (_,    null) { false };
      case (null, _)    { false };
      case (?nl, ?nr) {
	           switch (matchLeaf<K,V>(tl),
		                 matchLeaf<K,V>(tr)) {
	           case (null,  null)  {
                    rec(nl.left, nr.left)
					          and rec(nl.right, nr.right)
                  };
	           case (null,  _) { false };
	           case (_, null)  { false };
	           case (?kvs1, ?kvs2) {
                    List.isEq<(Key<K>,V)>
                    (kvs1, kvs2,
                     func ((k1:Key<K>, v1:V), (k2:Key<K>, v2:V)) : Bool =
                       keq(k1.key, k2.key) and veq(v1,v2)
                    )
                  };
	           }
           };
      }};
    rec(tl, tr)
  };

  // Equality function for two `Key<K>`s, in terms of equaltiy of `K`'s.
  func keyEq<K>(keq:(K,K) -> Bool) : ((Key<K>,Key<K>) -> Bool) = {
    func (key1:Key<K>, key2:Key<K>) : Bool =
      (Hash.hashEq(key1.hash, key2.hash) and keq(key1.key, key2.key))
  };

  /**
   Helpers for missing variants
   ==============================
   Until ActorScript has variant types, we need various helper functions here.  They are uninteresting.
   */
  // @Omit:

  // XXX: until AST-42:
  func isNull<X>(x : ?X) : Bool {
    switch x {
    case null { true  };
    case (?_) { false };
    };
  };

  // XXX: until AST-42:
  func assertIsNull<X>(x : ?X) {
    switch x {
    case null { assert(true)  };
    case (?_) { assert(false) };
    };
  };

  // XXX: until AST-42:
  func makeEmpty<K,V>() : Trie<K,V>
    = null;

   // XXX: until AST-42:
  func assertIsEmpty<K,V>(t : Trie<K,V>) {
    switch t {
    case null { assert(true)  };
    case (?_) { assert(false) };
    };
  };

  // XXX: until AST-42:
  func makeBin<K,V>(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V>  {
    ?(new {left=l; right=r; keyvals=null; })
  };

  // XXX: until AST-42:
  func isBin<K,V>(t:Trie<K,V>) : Bool {
    switch t {
    case null { false };
    case (?t_) {
	         switch (t_.keyvals) {
	         case null { true };
	         case _ { false };
	         };
	       };
    }
  };

  // XXX: until AST-42:
  func makeLeaf<K,V>(kvs:AssocList<Key<K>,V>) : Trie<K,V> {
    ?(new {left=null; right=null; keyvals=kvs })
  };

  // XXX: until AST-42:
  func matchLeaf<K,V>(t:Trie<K,V>) : ?AssocList<Key<K>,V> {
    switch t {
    case null { null };
    case (?t_) {
	         switch (t_.keyvals) {
	         case (?keyvals) ?(?(keyvals));
	         case (_) null;
	         }
	       };
    }
  };

  // XXX: until AST-42:
  func isLeaf<K,V>(t:Trie<K,V>) : Bool {
    switch t {
    case null { false };
    case (?t_) {
	         switch (t_.keyvals) {
	         case null { false };
	         case _ { true };
	         }
	       };
    }
  };
  // XXX: until AST-42:
  func assertIsBin<K,V>(t : Trie<K,V>) {
    switch t {
    case null { assert(false) };
    case (?n) {
	         assertIsNull<((Key<K>,V),AssocList<Key<K>,V>)>(n.keyvals);
         };
    }
  };

  // XXX: until AST-42:
  func getLeafKey<K,V>(t : Node<K,V>) : Key<K> {
    assertIsNull<Node<K,V>>(t.left);
    assertIsNull<Node<K,V>>(t.right);
    switch (t.keyvals) {
    case (?((k,v),_)) { k };
    case (null) { /* ERROR */ getLeafKey<K,V>(t) };
    }
  };

  // XXX: this helper is an ugly hack; we need real sum types to avoid it, I think:
  func getLeafVal<K,V>(t : Node<K,V>) : V {
    assertIsNull<Node<K,V>>(t.left);
    assertIsNull<Node<K,V>>(t.right);
    switch (t.keyvals) {
    case (?((k,v),_)) { v };
    case null { /* ERROR */ getLeafVal<K,V>(t) };
    }
  };


  /**
   More helpers
   ==============================
   */


  /**
   `buildNewPath`
   ---------------
   helper function for constructing new paths of uniform length
   */

  func buildNewPath<K,V>(bitpos:Nat, k:Key<K>, ov:?V) : Trie<K,V> {
    func rec(bitpos:Nat) : Trie<K,V> {
      if ( bitpos < HASH_BITS ) {
	      // create new bin node for this bit of the hash
	      let path = rec(bitpos+1);
	      let bit = Hash.getHashBit(k.hash, bitpos);
	      if (not bit) {
	        ?(new {left=path; right=null; keyvals=null})
	      }
	      else {
	        ?(new {left=null; right=path; keyvals=null})
	      }
      } else {
	      // create new leaf for (k,v) pair, if the value is non-null:
        switch ov {
          case null { ?(new {left=null; right=null; keyvals=null }) };
          case (?v) { ?(new {left=null; right=null; keyvals=?((k,v),null) }) };
        }
      }
    };
    rec(bitpos)
  };

//};


/**

Future work
=============

Tests
---------
more regression tests for everything documented in the [module interface](#module-interface).


Variant types
------------------------
See [AST-42]() (sum types); we want this type definition instead:

 ```
 // Use a sum type (AST-42)
 type Trie<K,V>     = { #leaf : LeafNode<K,V>; #bin : BinNode<K,V>; #empty };
 type BinNode<K,V>  = { left:Trie<K,V>; right:Trie<K,V> };
 type LeafNode<K,V> = { key:K; val:V };
 ```

Adaptive path lengths
----------------------

Currently we assume a uniform path length.  This can be inefficient,
and requires careful tuning.  In the future, we could adapt the path
length of each subtree to its cardinality; this wouild avoid
needlessly long paths, or paths that are too short for their subtree's
size.

Iterator objects
-------------------
for use in 'for ... in ...' patterns

*/
