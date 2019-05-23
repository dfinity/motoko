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

Below, we define the types used in the representation:

 - **`Key<K>`**, parameterized by a hashable type `K`


See the full details in the definitions below:

*/

let P = (import "prelude.as");

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

type Trie<K,V> = {
  #empty  ;
  #leaf   : AssocList<Key<K>,V>    ;
  #branch : (Trie<K,V>, Trie<K,V>) ;
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
            ((#leaf(kvs)), null)
          };
	   case (#branch b) {
	          let bit = Hash.getHashBit(k.hash, bitpos);
	          // rebuild either the left or right path with the inserted (k,v) pair
	          if (not bit) {
	            let (l, v_) = rec(b.0, bitpos+1);
	            (#branch(l, b.1), v_)
	          }
	          else {
	            let (r, v_) = rec(b.1, bitpos+1);
	            (#branch(b.0, r), v_)
	          }
	        };
     case (#leaf al) {
            let (kvs2, old_val) =
              AssocList.replace<Key<K>,V>(al, k, key_eq, v);
	          (#leaf(kvs2), old_val)
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
       case (#leaf as) { 
              label profile_trie_find_end_assocList_find : (?V)
              AssocList.find<Key<K>,V>(as, k, key_eq)
            };
       case (#branch(l,r)) {
	            let bit = Hash.getHashBit(k.hash, bitpos);
	            if (not bit) { 
                label profile_trie_find_branch_left : (?V)                
                rec(l, bitpos+1) 
              }
	            else { 
                label profile_trie_find_branch_right : (?V)
                rec(r, bitpos+1) 
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
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      switch (tl, tr) {
        case (#empty, _) { return tr };
        case (_, #empty) { return tl };
        case (#leaf as, #leaf bs) { 
               #leaf(
                 AssocList.disj<Key<K>,V,V,V>(
                   as, bs,
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
        case (#leaf al, _) {
               let (l,r) = splitAssocList<K,V>(al, bitpos);
               rec(bitpos, #branch(#leaf l, #leaf r), tr)
             };
        case (_, #leaf al) {
               let (l,r) = splitAssocList<K,V>(al, bitpos);
               rec(bitpos, tl, #branch(#leaf l, #leaf r))
             };
        case (#branch (ll, lr), #branch(rl, rr)) {
               #branch(rec(bitpos + 1, ll, rl), 
                       rec(bitpos + 1, lr, rr))
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
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
      switch (tl, tr) {
        case (#empty, _) { return tr };
        case (_, #empty) { return tl };
        case (#leaf as, #leaf bs) { 
               #leaf(
                 AssocList.disj<Key<K>,V,V,V>(
                   as, bs,
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
        case (#leaf al, _) {
               let (l,r) = splitAssocList<K,V>(al, bitpos);
               rec(bitpos, #branch(#leaf l, #leaf r), tr)
             };
        case (_, #leaf al) {
               let (l,r) = splitAssocList<K,V>(al, bitpos);
               rec(bitpos, tl, #branch(#leaf l, #leaf r))
             };
        case (#branch (ll, lr), #branch(rl, rr)) {
               #branch(rec(bitpos + 1, ll, rl), 
                       rec(bitpos + 1, lr, rr))
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
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,V> {
      switch (tl, tr) {
        case (#empty, _) { return #empty };
        case (_, #empty) { return tl };
        case (#leaf as, #leaf bs) { 
               #leaf(
                 AssocList.diff<Key<K>,V,W>(
                   as, bs,
                   key_eq,
                 )
               )
             };
        case (#leaf al, _) {
               let (l,r) = splitAssocList<K,V>(al, bitpos);
               rec(bitpos, #branch(#leaf l, #leaf r), tr)
             };
        case (_, #leaf al) {
               let (l,r) = splitAssocList<K,W>(al, bitpos);
               rec(bitpos, tl, #branch(#leaf l, #leaf r))
             };
        case (#branch (ll, lr), #branch(rl, rr)) {
               #branch(rec(bitpos + 1, ll, rl), 
                       rec(bitpos + 1, lr, rr))
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
    func recL(t:Trie<K,V>) : Trie<K,X> {
      switch t {
	    case (#empty) #empty;
	    case (#leaf as) {
             #leaf(AssocList.disj<Key<K>,V,W,X>(as, null, key_eq, vbin))
           };
      case (#branch(l,r)) { #branch(recL(l),recL(r)) };
      }
    };
    func recR(t:Trie<K,W>) : Trie<K,X> {
      switch t {
	    case (#empty) #empty;
	    case (#leaf as) {
             #leaf(AssocList.disj<Key<K>,V,W,X>(null, as, key_eq, vbin))
           };
      case (#branch(l,r)) { #branch(recR(l),recR(r)) };
      }
    };
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      switch (tl, tr) {
      case (#empty, #empty) { #empty };
      case (#empty, _   )   { recR(tr) };
      case (_,    #empty)   { recL(tl) };
      case (#leaf as, #leaf bs) {
             #leaf(AssocList.disj<Key<K>,V,W,X>(as, bs, key_eq, vbin))
           };
      case (#leaf al, _) {
             let (l,r) = splitAssocList<K,V>(al, bitpos);
             rec(bitpos, #branch(#leaf l, #leaf r), tr)
           };
      case (_, #leaf al) {
             let (l,r) = splitAssocList<K,W>(al, bitpos);
             rec(bitpos, tl, #branch(#leaf l, #leaf r))
           };
      case (#branch (ll, lr), #branch(rl, rr)) {
             #branch(rec(bitpos + 1, ll, rl), 
                     rec(bitpos + 1, lr, rr))
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
    func rec(bitpos:Nat, tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
      switch (tl, tr) {
	    case (#empty, _) { #empty };
	    case (_, #empty) { #empty };
	    case (#leaf as, #leaf bs) { 
             #leaf(AssocList.join<Key<K>,V,W,X>(as, bs, key_eq, vbin))
           };
      case (#leaf al, _) {
             let (l,r) = splitAssocList<K,V>(al, bitpos);
             rec(bitpos, #branch(#leaf l, #leaf r), tr)
           };
      case (_, #leaf al) {
             let (l,r) = splitAssocList<K,W>(al, bitpos);
             rec(bitpos, tl, #branch(#leaf l, #leaf r))
           };
      case (#branch (ll, lr), #branch(rl, rr)) {
             #branch(rec(bitpos + 1, ll, rl), 
                     rec(bitpos + 1, lr, rr))
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
	    case (#leaf al) {
             AssocList.fold<Key<K>,V,X>(
               al, empty,
               func (k:Key<K>, v:V, x:X):X =
                 bin(leaf(k.key,v),x)
             )
           };
	    case (#branch(l,r)) { bin(rec(l), rec(r)) };
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
	    case (#leaf al) {
             AssocList.fold<Key<K>,V,X>(
               al, x,
               func (k:Key<K>, v:V, x:X):X = f(k.key,v,x)
             )
           };
	    case (#branch(l,r)) { rec(l,rec(r,x)) };
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
	    case (#leaf al) {
             List.exists<(Key<K>,V)>(
               al, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
             )
           };
	    case (#branch(l,r)) { rec(l) or rec(r) };
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
	    case (#leaf al) {
             List.all<(Key<K>,V)>(
               al, func ((k:Key<K>,v:V)):Bool=f(k.key,v)
             )
           };
	    case (#branch(l,r)) { rec(l) and rec(r) };
      };
    };
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
  func toArray<K,V,W>(t:Trie<K,V>,f:(K,V)->[W]):[W] =
    label profile_trie_toArray_begin : [W] {
    func arrayAppend(x:[W],y:[W]):[W] {
      label profile_trie_toArray_arrayAppend : [W]
      Array_tabulate<W> (
        x.len() + y.len(),
        func (i:Nat) : W = label profile_trie_toArray_arrayAppend_projelm : W {
          if (i >= x.len()) { y[i - x.len()] }
          else { x[i] }
        }
      )
    };
    let result = foldUp<K,V,[W]>
    (t,
     arrayAppend,
     func(k:K, v:V):[W]{f(k,v)},
     []);
    label profile_trie_toArray_end : [W]
    result
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
      case (#empty) { true };
	    case (#leaf al) { List.isNil<(Key<K>,V)>(al) };
	    case (#branch(l,r)) { rec(l) and rec(r) };
	    };
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
      case (#empty) { #empty };
	    case (#leaf al) {
             #leaf(List.filter<(Key<K>,V)>(al, func ((k:Key<K>,v:V)):Bool = f(k.key,v)))
           };
	    case (#branch(l,r)) {
		         let fl = rec(l);
		         let fr = rec(r);
		         switch (isEmpty<K,V>(fl),
			               isEmpty<K,V>(fr)) {
		         case (true,  true)  #empty;
		         case (false, true)  fr;
		         case (true,  false) fl;
		         case (false, false) #branch(fl, fr);
		         };
	         }
      }
    };
    rec(t)
  };

  /**
   `mapFilter`
   -----------
   map and filter the key-value pairs by a given predicate.
   */
  func mapFilter<K,V,W>(t:Trie<K,V>, f:(K,V)->?W) : Trie<K,W> {
    func rec(t:Trie<K,V>) : Trie<K,W> {
      switch t {
      case (#empty) { #empty };
	    case (#leaf al) {
             #leaf(List.mapFilter<(Key<K>,V),(Key<K>,W)>(
                     al, 
                     // retain key and hash, but update key's value using f:
                     func ((k:Key<K>,v:V)):?(Key<K>,W) = {
                       switch (f(k.key,v)) {
                         case (null) null;
                         case (?w) (?(new {key=k.key; hash=k.hash}, w));
                       }}
                   )
             )
           };
	    case (#branch(l,r)) {
		         let fl = rec(l);
		         let fr = rec(r);
		         switch (isEmpty<K,W>(fl),
			               isEmpty<K,W>(fr)) {
		         case (true,  true)  #empty;
		         case (false, true)  fr;
		         case (true,  false) fl;
		         case (false, false) #branch(fl, fr);
		         };
	         }
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
      case (#empty, #empty) { true };
      case (#leaf as, #leaf bs) {
             List.isEq<(Key<K>,V)>
             (as, bs,
              func ((k1:Key<K>, v1:V), (k2:Key<K>, v2:V)) : Bool =
                keq(k1.key, k2.key) and veq(v1,v2)
             )
           };
      case (#branch(ll,lr),#branch(rl,rr)) {
             rec(ll,rl) and rec(lr,rr)
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
