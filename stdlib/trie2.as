module{
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

*/

/**
Representation
=====================

A hash trie is a binary trie, where each (internal) branch node
represents having distinguished its key-value pairs on a single bit of
the keys.

By following paths in the trie, we determine an increasingly smaller
and smaller subset of the keys.

Each leaf node consists of an association list of key-value pairs.

We say that a leaf is valid if it contains no more than MAX_LEAF_COUNT
key-value pairs.

Each non-empty trie node stores a count; we discuss that more below.

### Adaptive depth

For small mappings, this "trie" structure just consists of a single
leaf, which contains up to MAX_LEAF_COUNT key-value pairs.

By construction, the algorithms below enforce the invariant that no
leaf ever contains more than MAX_LEAF_COUNT key-value pairs: the
function `leaf` accepts a list, but subdivides it with branches until
it can actually construct valid leaves.  Once distinguished, subsets
of keys tend to remain distinguished by the presence of these branches.

To do: Collapse branches whose count is less than MAX_LEAF_COUNT.

### Cached counts

At each branch and leaf, we use a stored count to support a
memory-efficient `toArray` function, which itself relies on
per-element projection via `nth`; in turn, `nth` directly uses the
O(1)-time function `count` for achieving an acceptable level of
algorithmic efficiently.  Notably, leaves are generally lists of
key-value pairs, and we do not store a count for each Cons cell in the
list.


### Details

Below, we define the types used in the representation:

*/

let MAX_LEAF_COUNT = 32;

let P = (import "prelude.as");

let Option = import "option.as";

let Hash = (import "hash.as").BitVec;
type Hash = Hash.t;

let List = import "list.as";
type List<T> = List.List<T>;

let AssocList = import "assocList.as";
type AssocList<K,V> = AssocList.AssocList<K,V>;

type Key<K> = {
  // hash field: permits fast inequality checks, permits collisions;
  // (eventually: permits incremental growth of deeper trie paths)
  hash: Hash;
  // key field: for conservative equality checks, after equal hashes.
  key: K;
};

// Equality function for two `Key<K>`s, in terms of equaltiy of `K`'s.
func keyEq<K>(keq:(K,K) -> Bool) : ((Key<K>,Key<K>) -> Bool) = {
  func (key1:Key<K>, key2:Key<K>) : Bool =
    label profile_trie_keyEq : Bool
  (Hash.hashEq(key1.hash, key2.hash) and keq(key1.key, key2.key))
};

type Leaf<K,V> = {
  count   : Nat ;
  keyvals : AssocList<Key<K>,V> ;
};

type Branch<K,V> = {
  count : Nat ;
  left  : Trie<K,V> ;
  right : Trie<K,V> ;
};

type Trie<K,V> = {
  #empty  ;
  #leaf   : Leaf<K,V> ;
  #branch : Branch<K,V> ;
};

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
 */

 /**
  `empty`
  --------
  An empty trie.
  */
 func empty<K,V>() : Trie<K,V> = #empty;

 /**
  `count`
  --------
  Get the number of key-value pairs in the trie, in constant time.

  ### Implementation notes

  `nth` directly uses this function `count` for achieving an
  acceptable level of algorithmic efficiently.

  */
 func count<K,V>(t: Trie<K,V>) : Nat {
   switch t {
     case (#empty) 0;
     case (#leaf l) l.count;
     case (#branch b) b.count;
   }
 };


 /**
  `branch`
  --------
  Construct a branch node, computing the count stored there.
  */
 func branch<K,V>(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> {
   // note: need to rename the `count` function, because otherwise AS
   // is confused by what looks like a recursive definition below for
   // the field `count`.
   func c(t:Trie<K,V>) : Nat = count<K,V>(t);
   #branch(
     new{
       count=c(l) + c(r);
       left=l;
       right=r
     }
   )
 };

 /**
  `leaf`
  --------
  Construct a leaf node, computing the count stored there.

  This helper function automatically enforces the MAX_LEAF_COUNT
  by constructing branches as necessary; to do so, it also needs the bitpos
  of the leaf.

  */
 func leaf<K,V>(kvs:AssocList<Key<K>,V>, bitpos:Nat) : Trie<K,V> {
   func rec(kvs:AssocList<Key<K>,V>, bitpos:Nat) : Trie<K,V> {
     let len = List.len<(Key<K>,V)>(kvs);
     if ( len < MAX_LEAF_COUNT ) {
       #leaf(
         new{
           count=len;
           keyvals=kvs
         })
     } else {
       let (l, r) = splitAssocList<K,V>(kvs, bitpos);
       branch<K,V>(rec(l, bitpos + 1), rec(r, bitpos + 1))
     }
   };
   rec(kvs, bitpos)
 };

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
   func rec(t : Trie<K,V>, bitpos:Nat) : (Trie<K,V>, ?V) {
	   switch t {
	   case (#empty) {
            let (kvs, _) = AssocList.replace<Key<K>,V>(null, k, key_eq, v);
            (leaf<K,V>(kvs, bitpos), null)
          };
	   case (#branch b) {
	          let bit = Hash.getHashBit(k.hash, bitpos);
	          // rebuild either the left or right path with the inserted (k,v) pair
	          if (not bit) {
	            let (l, v_) = rec(b.left, bitpos+1);
	            (branch<K,V>(l, b.right), v_)
	          }
	          else {
	            let (r, v_) = rec(b.right, bitpos+1);
	            (branch<K,V>(b.left, r), v_)
	          }
	        };
     case (#leaf l) {
            let (kvs2, old_val) =
              AssocList.replace<Key<K>,V>(l.keyvals, k, key_eq, v);
            (leaf<K,V>(kvs2, bitpos), old_val)
          };
     }
   };
   rec(t, 0)
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
  `find`
  ---------
  find the given key's value in the trie, or return null if nonexistent
  */
 func find<K,V>(t : Trie<K,V>, k:Key<K>, k_eq:(K,K) -> Bool) : ?V {
   let key_eq = keyEq<K>(k_eq);
   func rec(t : Trie<K,V>, bitpos:Nat) : ?V = label profile_trie_find_rec : (?V) {
     switch t {
       case (#empty) {
              label profile_trie_find_end_null : (?V)
              null
            };
       case (#leaf l) {
              label profile_trie_find_end_assocList_find : (?V)
              AssocList.find<Key<K>,V>(l.keyvals, k, key_eq)
            };
       case (#branch b) {
	            let bit = Hash.getHashBit(k.hash, bitpos);
	            if (not bit) {
                label profile_trie_find_branch_left : (?V)
                rec(b.left, bitpos+1)
              }
	            else {
                label profile_trie_find_branch_right : (?V)
                rec(b.right, bitpos+1)
              }
             };
     }
   };
   label profile_trie_find_begin : (?V)
   rec(t, 0)
 };




 func splitAssocList<K,V>(al:AssocList<Key<K>,V>, bitpos:Nat)
   : (AssocList<Key<K>,V>, AssocList<Key<K>,V>)
 {
   List.split<(Key<K>,V)>(
     al,
     func ((k : Key<K>, v : V)) : Bool{
       Hash.getHashBit(k.hash, bitpos)
     }
   )
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
    func br(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> = branch<K,V>(l,r);
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      func lf(kvs:AssocList<Key<K>,V>) : Trie<K,V> = leaf<K,V>(kvs, bitpos);
      switch (tl, tr) {
        case (#empty, _) { return tr };
        case (_, #empty) { return tl };
        case (#leaf l1, #leaf l2) {
               lf(
                 AssocList.disj<Key<K>,V,V,V>(
                   l1.keyvals, l2.keyvals,
                   key_eq,
                   func (x:?V, y:?V):V = {
                     switch (x, y) {
                     case (null, null) { P.unreachable() };
                     case (null, ?v) v;
                     case (?v, _) v;
                     }}
                 )
               )
             };
        case (#leaf l, _) {
               let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
               rec(bitpos, br(lf ll, lf lr), tr)
             };
        case (_, #leaf l) {
               let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
               rec(bitpos, tl, br(lf ll, lf lr))
             };
        case (#branch b1, #branch b2) {
               br(rec(bitpos + 1, b1.left, b2.left),
                  rec(bitpos + 1, b1.right, b2.right))
             };
      }
    };
    rec(0, tl, tr)
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
    func br(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> = branch<K,V>(l,r);
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      func lf(kvs:AssocList<Key<K>,V>) : Trie<K,V> = leaf<K,V>(kvs, bitpos);
      switch (tl, tr) {
        case (#empty, _) { return tr };
        case (_, #empty) { return tl };
        case (#leaf l1, #leaf l2) {
               lf(
                 AssocList.disj<Key<K>,V,V,V>(
                   l1.keyvals, l2.keyvals,
                   key_eq,
                   func (x:?V, y:?V):V = {
                     switch (x, y) {
                     case (null, null) {
                            /* IMPOSSIBLE case. */
                            P.unreachable()
                          };
                     case (?_, ?_) {
                            /* INVALID case: left and right defined for the same key */
                            assert false;
                            P.unreachable()
                          };
                     case (null, ?v) v;
                     case (?v, null) v;
                     }}
                 )
               )
             };
        case (#leaf l, _) {
               let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
               rec(bitpos, br(lf ll, lf lr), tr)
             };
        case (_, #leaf l) {
               let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
               rec(bitpos, tl, br(lf ll, lf lr))
             };
        case (#branch b1, #branch b2) {
               br(rec(bitpos + 1, b1.left, b2.left),
                  rec(bitpos + 1, b1.right, b2.right))
             };

      }
    };
    rec(0, tl, tr)
  };

  /**
   `diff`
   ------
   The key-value pairs of the final trie consists of those pairs of
   the left trie whose keys are not present in the right trie; the
   values of the right trie are irrelevant.
   */
  func diff<K,V,W>(tl:Trie<K,V>, tr:Trie<K,W>, k_eq:(K,K)->Bool): Trie<K,V> {
    let key_eq = keyEq<K>(k_eq);

    func br1(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> = branch<K,V>(l,r);
    func br2(l:Trie<K,W>, r:Trie<K,W>) : Trie<K,W> = branch<K,W>(l,r);

    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,V> {
      func lf1(kvs:AssocList<Key<K>,V>) : Trie<K,V> = leaf<K,V>(kvs, bitpos);
      func lf2(kvs:AssocList<Key<K>,W>) : Trie<K,W> = leaf<K,W>(kvs, bitpos);

      switch (tl, tr) {
        case (#empty, _) { return #empty };
        case (_, #empty) { return tl };
        case (#leaf l1, #leaf l2) {
               lf1(
                 AssocList.diff<Key<K>,V,W>(
                   l1.keyvals, l2.keyvals,
                   key_eq,
                 )
               )
             };
        case (#leaf l, _) {
               let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
               rec(bitpos, br1(lf1 ll, lf1 lr), tr)
             };
        case (_, #leaf l) {
               let (ll, lr) = splitAssocList<K,W>(l.keyvals, bitpos);
               rec(bitpos, tl, br2(lf2 ll, lf2 lr))
             };
        case (#branch b1, #branch b2) {
               br1(rec(bitpos + 1, b1.left, b2.left),
                   rec(bitpos + 1, b1.right, b2.right))
             };
      }
    };
    rec(0, tl, tr)
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
  func disj<K,V,W,X>(
    tl   : Trie<K,V>,
    tr   : Trie<K,W>,
		k_eq : (K,K)->Bool,
    vbin : (?V,?W)->X
  )
    : Trie<K,X>
  {
    let key_eq = keyEq<K>(k_eq);

    func br1(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> = branch<K,V>(l,r);
    func br2(l:Trie<K,W>, r:Trie<K,W>) : Trie<K,W> = branch<K,W>(l,r);

    func br3(l:Trie<K,X>, r:Trie<K,X>) : Trie<K,X> = branch<K,X>(l,r);
    func lf3(kvs:AssocList<Key<K>,X>, bitpos:Nat) : Trie<K,X> = leaf<K,X>(kvs, bitpos);

    /** empty right case; build from left only: */
    func recL(t:Trie<K,V>, bitpos:Nat) : Trie<K,X> {
      switch t {
	    case (#empty) #empty;
	    case (#leaf l) {
             lf3(AssocList.disj<Key<K>,V,W,X>(l.keyvals, null, key_eq, vbin), bitpos)
           };
      case (#branch(b)) { br3(recL(b.left,bitpos+1),recL(b.right,bitpos+1)) };
      }
    };
    /** empty left case; build from right only: */
    func recR(t:Trie<K,W>, bitpos:Nat) : Trie<K,X> {
      switch t {
	    case (#empty) #empty;
	    case (#leaf l) {
             lf3(AssocList.disj<Key<K>,V,W,X>(null, l.keyvals, key_eq, vbin), bitpos)
           };
      case (#branch(b)) { br3(recR(b.left,bitpos+1),recR(b.right,bitpos+1)) };
      }
    };

    /** main recursion */
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      func lf1(kvs:AssocList<Key<K>,V>) : Trie<K,V> = leaf<K,V>(kvs, bitpos);
      func lf2(kvs:AssocList<Key<K>,W>) : Trie<K,W> = leaf<K,W>(kvs, bitpos);
      switch (tl, tr) {
      case (#empty, #empty) { #empty };
      case (#empty, _   )   { recR(tr, bitpos) };
      case (_,    #empty)   { recL(tl, bitpos) };
      case (#leaf l1, #leaf l2) {
             lf3(AssocList.disj<Key<K>,V,W,X>(l1.keyvals, l2.keyvals, key_eq, vbin), bitpos)
           };
      case (#leaf l, _) {
             let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
             rec(bitpos, br1(lf1 ll, lf1 lr), tr)
           };
      case (_, #leaf l) {
             let (ll, lr) = splitAssocList<K,W>(l.keyvals, bitpos);
             rec(bitpos, tl, br2(lf2 ll, lf2 lr))
           };
      case (#branch b1, #branch b2) {
             br3(rec(bitpos + 1, b1.left, b2.left),
                 rec(bitpos + 1, b1.right, b2.right))
           };

      }
    };

    rec(0, tl, tr)
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

    func br1(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V> = branch<K,V>(l,r);
    func br2(l:Trie<K,W>, r:Trie<K,W>) : Trie<K,W> = branch<K,W>(l,r);
    func br3(l:Trie<K,X>, r:Trie<K,X>) : Trie<K,X> = branch<K,X>(l,r);

    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      func lf1(kvs:AssocList<Key<K>,V>) : Trie<K,V> = leaf<K,V>(kvs, bitpos);
      func lf2(kvs:AssocList<Key<K>,W>) : Trie<K,W> = leaf<K,W>(kvs, bitpos);
      func lf3(kvs:AssocList<Key<K>,X>) : Trie<K,X> = leaf<K,X>(kvs, bitpos);

      switch (tl, tr) {
	    case (#empty, _) { #empty };
	    case (_, #empty) { #empty };
	    case (#leaf l1, #leaf l2) {
             lf3(AssocList.join<Key<K>,V,W,X>(l1.keyvals, l2.keyvals, key_eq, vbin))
           };
      case (#leaf l, _) {
             let (ll, lr) = splitAssocList<K,V>(l.keyvals, bitpos);
             rec(bitpos, br1(lf1 ll, lf1 lr), tr)
           };
      case (_, #leaf l) {
             let (ll, lr) = splitAssocList<K,W>(l.keyvals, bitpos);
             rec(bitpos, tl, br2(lf2 ll, lf2 lr))
           };
      case (#branch b1, #branch b2) {
             br3(rec(bitpos + 1, b1.left, b2.left),
                 rec(bitpos + 1, b1.right, b2.right))
           };

      }
    };
    rec(0, tl, tr)
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
      case (#empty) { empty };
	    case (#leaf l) {
             AssocList.fold<Key<K>,V,X>(
               l.keyvals, empty,
               func (k:Key<K>, v:V, x:X):X =
                 bin(leaf(k.key,v),x)
             )
           };
	    case (#branch(b)) { bin(rec(b.left), rec(b.right)) };
      }
    };
    rec(t)
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
            case null #empty;
            case (?(k3, v3)) { (insert<K3, V3>(#empty, k3, k3_eq, v3)).0 };
            }
          },
          #empty
        )
      },
      #empty
    )
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
      case (#empty) x;
	    case (#leaf l) {
             AssocList.fold<Key<K>,V,X>(
               l.keyvals, x,
               func (k:Key<K>, v:V, x:X):X = f(k.key,v,x)
             )
           };
	    case (#branch(b)) { rec(b.left,rec(b.right,x)) };
      };
    };
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
      case (#empty) { false };
	    case (#leaf l) {
             List.exists<(Key<K>,V)>(
               l.keyvals, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
             )
           };
	    case (#branch(b)) { rec(b.left) or rec(b.right) };
      };
    };
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
      case (#empty) { true };
	    case (#leaf l) {
             List.all<(Key<K>,V)>(
               l.keyvals, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
             )
           };
	    case (#branch(b)) { rec(b.left) and rec(b.right) };
      };
    };
    rec(t)
  };

  /**
   `nth`
   --------
   Project the nth key-value pair from the trie.

   Note: This position is not meaningful; it's only here so that we
   can inject tries into arrays given the Array_tabulate interface for
   doing so.
   */
  func nth<K,V>(t:Trie<K,V>, i:Nat) : ?(Key<K>, V) {
    func rec(t:Trie<K,V>, i:Nat) : ?(Key<K>, V) {
      switch t {
      case (#empty) null;
      case (#leaf l) List.nth<(Key<K>,V)>(l.keyvals, i);
      case (#branch b) {
             let count_left = count<K,V>(b.left);
             if (i < count_left) { rec(b.left,  i) }
             else                { rec(b.right, i - count_left) }
           }
      }
    };
    assert (i < count<K,V>(t));
    rec(t, i)
  };

  /**
   `toArray`
   --------
   Gather the collection of key-value pairs into an array of a (possibly-distinct) type.

   ### Implementation notes:

   we use this function repeatedly in the Produce Exchange example
   application, often on very large tries.

   Performance Profiling shows that it is important that this be
   memory efficient, and reasonably time efficient, at large scales.

   To do so, we use a single array allocation: The returned array.  To do so, we
   sacrifice some efficiency in reading the input trie, and use function `nth` to
   project each element with an independent trie traversal.

   This approach is somewhat forced on us by the type signature of
   Array_tabulate, and the desire to only allocate one array; that requirement rules
   out iterative mutation of an optionally-null array, since an imperative
   approach which would give us the wrong return type.

   Since we want to  statically rule out null output elements, and since the AS type system
   cannot do that for an imperative approach unless we assume more about
   the type W (e.g., the existence of "default values"), we settle for using `nth`.

   */
  func toArray<K,V,W>(t:Trie<K,V>,f:(K,V)->W):[W] =
    label profile_trie_toArray_begin : [W] {
    let a = Array_tabulate<W> (
      count<K,V>(t),
      func (i:Nat) : W = label profile_trie_toArray_nth : W {
        let (k,v) = Option.unwrap<(Key<K>,V)>(nth<K,V>(t, i));
        f(k.key, v)
      }
    );
    label profile_trie_toArray_end : [W]
    a
  };

  /**
   `isEmpty`
   -----------
   specialized foldUp operation.
   Test for "deep emptiness": subtrees that have branching structure,
   but no leaves.  These can result from naive filtering operations;
   filter uses this function to avoid creating such subtrees.
   */
  func isEmpty<K,V>(t:Trie<K,V>) : Bool =
    count<K,V>(t) == 0;

  /**
   `filter`
   -----------
   filter the key-value pairs by a given predicate.
   */
  func filter<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Trie<K,V> {
    func rec(t:Trie<K,V>, bitpos:Nat) : Trie<K,V> {
      switch t {
      case (#empty) { #empty };
	    case (#leaf l) {
             leaf<K,V>(
               List.filter<(Key<K>,V)>(
                 l.keyvals,
                 func ((k:Key<K>,v:V)):Bool = f(k.key,v)
               ),
               bitpos
             )
           };
	    case (#branch(b)) {
		         let fl = rec(b.left, bitpos+1);
		         let fr = rec(b.right, bitpos+1);
		         switch (isEmpty<K,V>(fl),
			               isEmpty<K,V>(fr)) {
		         case (true,  true)  #empty;
		         case (false, true)  fr;
		         case (true,  false) fl;
		         case (false, false) branch<K,V>(fl, fr);
		         };
	         }
      }
    };
    rec(t, 0)
  };

  /**
   `mapFilter`
   -----------
   map and filter the key-value pairs by a given predicate.
   */
  func mapFilter<K,V,W>(t:Trie<K,V>, f:(K,V)->?W) : Trie<K,W> {
    func rec(t:Trie<K,V>, bitpos:Nat) : Trie<K,W> {
      switch t {
      case (#empty) { #empty };
	    case (#leaf l) {
             leaf<K,W>(
               List.mapFilter<(Key<K>,V),(Key<K>,W)>(
                 l.keyvals,
                 // retain key and hash, but update key's value using f:
                 func ((k:Key<K>,v:V)):?(Key<K>,W) = {
                   switch (f(k.key,v)) {
                   case (null) null;
                   case (?w) (?(new {key=k.key; hash=k.hash}, w));
                   }}
               ),
               bitpos
             )
           };
	    case (#branch(b)) {
		         let fl = rec(b.left, bitpos + 1);
		         let fr = rec(b.right, bitpos + 1);
		         switch (isEmpty<K,W>(fl),
			               isEmpty<K,W>(fr)) {
		         case (true,  true)  #empty;
		         case (false, true)  fr;
		         case (true,  false) fl;
		         case (false, false) branch<K,W>(fl, fr);
		         };
	         }
      }
    };
    rec(t, 0)
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
      case (#empty, #empty) { true };
      case (#leaf l1, #leaf l2) {
             List.isEq<(Key<K>,V)>
             (l1.keyvals, l2.keyvals,
              func ((k1:Key<K>, v1:V), (k2:Key<K>, v2:V)) : Bool =
                keq(k1.key, k2.key) and veq(v1,v2)
             )
           };
      case (#branch(b1),#branch(b2)) {
             rec(b1.left, b2.left) and rec(b2.right, b2.right)
           };
      case _ { false };
      }
    };
    rec(tl,tr)
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
    case (null)   { insert<K2,V>(#empty, k2, k2_eq, v) };
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
             #empty, k2, k2_eq,
             (insert<K3,V>(#empty, k3, k3_eq, v)).0
           )
         };
    case (?inner1) {
           let inner2 = find<K2,Trie<K3,V>>(inner1, k2, k2_eq);
           let (updated_inner2, _) = switch inner2 {
           case (null) { insert<K3,V>(#empty, k3, k3_eq, v) };
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
      #empty )
  };


/**

Future work
=============

Iterator objects
-------------------
for use in 'for ... in ...' patterns

*/
}
