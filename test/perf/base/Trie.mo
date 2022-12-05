/// Functional key-value hash maps.
///
/// Functional maps (and sets) whose representation is "canonical", and
/// independent of operation history (unlike other popular search trees).
///
/// The representation we use here comes from Section 6 of ["Incremental computation via function caching", Pugh & Teitelbaum](https://dl.acm.org/citation.cfm?id=75305).
///
/// ## User's overview
///
/// This module provides an applicative (functional) hash map.
/// Notably, each `put` produces a **new trie _and value being replaced, if any_**.
///
/// Those looking for a more familiar (imperative,
/// object-oriented) hash map should consider `TrieMap` or `HashMap` instead.
///
/// The basic `Trie` operations consist of:
/// - `put` - put a key-value into the trie, producing a new version.
/// - `get` - get a key's value from the trie, or `null` if none.
/// - `iter` - visit every key-value in the trie.
///
/// The `put` and `get` operations work over `Key` records,
/// which group the hash of the key with its non-hash key value.
///
/// ```motoko
/// import Trie "mo:base/Trie";
/// import Text "mo:base/Text";
///
/// type Trie<K, V> = Trie.Trie<K, V>;
/// type Key<K> = Trie.Key<K>;
///
/// func key(t: Text) : Key<Text> { { key = t; hash = Text.hash t } };
///
/// let t0 : Trie<Text, Nat> = Trie.empty();
/// let t1 : Trie<Text, Nat> = Trie.put(t0, key "hello", Text.equal, 42).0;
/// let t2 : Trie<Text, Nat> = Trie.put(t1, key "world", Text.equal, 24).0;
/// let n : ?Nat = Trie.put(t1, key "hello", Text.equal, 0).1;
/// assert (n == ?42);
/// ```
///

// ## Implementation overview
//
// A (hash) trie is a binary tree container for key-value pairs that
// consists of leaf and branch nodes.
//
// Each internal **branch node**
// represents having distinguished its key-value pairs on a single bit of
// the keys.
// By following paths in the trie, we determine an increasingly smaller
// and smaller subset of the keys.
//
// Each **leaf node** consists of an association list of key-value pairs.
//
// Each non-empty trie node stores a size; we discuss that more below.
//
// ### Adaptive depth
//
// We say that a leaf is valid if it contains no more than `MAX_LEAF_SIZE`
// key-value pairs.  When a leaf node grows too large, the
// binary tree produces a new internal binary node, and splits the leaf into
// a pair of leaves using an additional bit of their keys' hash strings.
//
// For small mappings, the trie structure consists of a single
// leaf, which contains up to MAX_LEAF_SIZE key-value pairs.
//
// ### Cached sizes
//
// At each branch and leaf, we use a stored size to support a
// memory-efficient `toArray` function, which itself relies on
// per-element projection via `nth`; in turn, `nth` directly uses the
// O(1)-time function `size` for achieving an acceptable level of
// algorithmic efficiency.  Notably, leaves are generally lists of
// key-value pairs, and we do not store a size for each Cons cell in the
// list.
//

import Debug "mo:base/Debug";

import Prim "mo:⛔";
import P "Prelude";
import Option "Option";
import Hash "Hash";
import A "Array";

import List "List";
import AssocList "AssocList";
import I "Iter";

module {

  let MAX_LEAF_SIZE = 8; // to do -- further profiling and tuning

  /// Binary hash tries: either empty, a leaf node, or a branch node
  public type Trie<K, V> = {
    #empty;
    #leaf : Leaf<K, V>;
    #branch : Branch<K, V>;
  };

  /// Leaf nodes of trie consist of key-value pairs as a list.
  public type Leaf<K, V> = {
    size : Nat ;
    keyvals : AssocList<Key<K>, V> ;
  };

  /// Branch nodes of the trie discriminate on a bit position of the keys' hashes.
  /// we never store this bitpos; rather,
  /// we enforce a style where this position is always known from context.
  public type Branch<K, V> = {
    size : Nat;
    left : Trie<K, V>;
    right : Trie<K, V>;
  };

  public type AssocList<K, V> = AssocList.AssocList<K, V>;

  //// A `Key` for the trie has an associated hash value
  public type Key<K> = {
    /// `hash` permits fast inequality checks, and permits collisions
    hash: Hash.Hash;
    /// `key` permits precise equality checks, but only used after equal hashes.
    key: K;
  };

  type List<T> = List.List<T>;

  /// Equality function for two `Key<K>`s, in terms of equality of `K`'s.
  public func equalKey<K>(keq : (K, K) -> Bool) : ((Key<K>, Key<K>) -> Bool) {
    func (key1 : Key<K>, key2 : Key<K>) : Bool {
      Hash.equal(key1.hash, key2.hash) and keq(key1.key, key2.key)
    }
  };

  /// @deprecated `isValid` is an internal predicate and will be removed in future.
  public func isValid<K, V>(t : Trie<K, V>, _enforceNormal : Bool) : Bool {
    func rec(t : Trie<K, V>, bitpos : ?Hash.Hash, bits : Hash.Hash, mask : Hash.Hash) : Bool {
      switch t {
        case (#empty) {
          true;
        };
        case (#leaf(l)) {
          let len = List.size(l.keyvals);
            len <= MAX_LEAF_SIZE + 1
          and
            len == l.size
          and
            List.all(
              l.keyvals,
              func ((k : Key<K>, v : V)) : Bool {
                ((k.hash & mask) == bits)
              }
            )
        };
        case (#branch(b)) {
          let bitpos1 =
            switch bitpos {
              case null  {Prim.natToNat32(0)};
              case (?bp) {Prim.natToNat32(Prim.nat32ToNat(bp) + 1)}
            };
          let mask1 = mask | (Prim.natToNat32(1) << bitpos1);
          let bits1 = bits | (Prim.natToNat32(1) << bitpos1);
          let sum = size(b.left) + size(b.right);
          (b.size == sum) and
          rec(b.left,  ?bitpos1, bits,  mask1) and
          rec(b.right, ?bitpos1, bits1, mask1)
        };
      }
    };
    rec(t, null, 0, 0)
  };


  /// A 2D trie maps dimension-1 keys to another
  /// layer of tries, each keyed on the dimension-2 keys.
  public type Trie2D<K1, K2, V> = Trie<K1, Trie<K2, V>>;

  /// A 3D trie maps dimension-1 keys to another
  /// layer of 2D tries, each keyed on the dimension-2 and dimension-3 keys.
  public type Trie3D<K1, K2, K3, V> = Trie<K1, Trie2D<K2, K3, V> >;

  /// An empty trie.
  public func empty<K, V>() : Trie<K, V> { #empty; };

  ///  Get the number of key-value pairs in the trie, in constant time.

  /// Get size in O(1) time.
  public func size<K, V>(t : Trie<K, V>) : Nat {
    switch t {
      case (#empty) { 0 };
      case (#leaf(l)) { l.size };
      case (#branch(b)) { b.size };
    }
  };

  /// Construct a branch node, computing the size stored there.
  public func branch<K, V>(l : Trie<K, V>, r : Trie<K, V>) : Trie<K, V> {
      let sum = size(l) + size(r);
      #branch {
        size = sum;
        left = l;
        right = r
      };
    };

  /// Construct a leaf node, computing the size stored there.
  ///
  /// This helper function automatically enforces the MAX_LEAF_SIZE
  /// by constructing branches as necessary; to do so, it also needs the bitpos
  /// of the leaf.
  public func leaf<K, V>(kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V> {
    fromList(null, kvs, bitpos)
  };

  module ListUtil {
    /* Deprecated: List.lenClamp */
    /// Return the list length unless the number of items in the list exceeds
    /// a maximum value. If the list length exceed the maximum, the function
    /// returns `null`.
    public func lenClamp<T>(l : List<T>, max : Nat) : ?Nat {
      func rec(l : List<T>, max : Nat, i : Nat) : ?Nat {
        switch l {
          case null { ?i };
          case (?(_, t)) {
            if ( i > max ) { null }
            else { rec(t, max, i + 1) }
          };
        }
      };
      rec(l, max, 0)
    };
  };

  /// Transform a list into a trie, splitting input list into small (leaf) lists, if necessary.
  public func fromList<K, V>(kvc : ?Nat, kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V> {
    func rec(kvc : ?Nat, kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V> {
      switch kvc {
        case null {
          switch (ListUtil.lenClamp(kvs, MAX_LEAF_SIZE)) {
            case null {} /* fall through to branch case. */;
            case (?len) {
              return #leaf({ size = len; keyvals = kvs })
            };
          }
        };
        case (?c) {
          if ( c == 0 ) {
            return #empty
          } else if ( c <= MAX_LEAF_SIZE ) {
            return #leaf({ size = c; keyvals = kvs })
          } else {
          /* fall through to branch case */
          }
        };
      };
      let (ls, l, rs, r) = splitList(kvs, bitpos);
      if ( ls == 0 and rs == 0 ) {
        #empty
      } else if (rs == 0 and ls <= MAX_LEAF_SIZE) {
        #leaf({ size = ls; keyvals = l })
      } else if (ls == 0 and rs <= MAX_LEAF_SIZE) {
        #leaf({ size = rs; keyvals = r })
      } else {
        branch(rec(?ls, l, bitpos + 1), rec(?rs, r, bitpos + 1))
      }
    };
    rec(kvc, kvs, bitpos)
  };

  /// Clone the trie efficiently, via sharing.
  ///
  /// Purely-functional representation permits _O(1)_ copy, via persistent sharing.
  public func clone<K, V>(t : Trie<K, V>) : Trie<K, V> = t;

  /// Replace the given key's value option with the given one, returning the previous one
  public func replace<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : ?V) : (Trie<K, V>, ?V) {
     let key_eq = equalKey(k_eq);

     func rec(t : Trie<K, V>, bitpos : Nat) : (Trie<K, V>, ?V) {
        switch t {
          case (#empty) {
            let (kvs, _) = AssocList.replace(null, k, key_eq, v);
            (leaf(kvs, bitpos), null)
          };
          case (#branch(b)) {
            let bit = Hash.bit(k.hash, bitpos);
            // rebuild either the left or right path with the (k, v) pair
            if (not bit) {
              let (l, v_) = rec(b.left, bitpos + 1);
              (branch(l, b.right), v_)
            }
            else {
              let (r, v_) = rec(b.right, bitpos + 1);
              (branch(b.left, r), v_)
            }
          };
       case (#leaf(l)) {
         let (kvs2, old_val) =
           AssocList.replace(l.keyvals, k, key_eq, v);
         (leaf(kvs2, bitpos), old_val)
        };
      }
     };
     let (to, vo) = rec(t, 0);
   //assert(isValid<K, V>(to, false));
     (to, vo)
   };

  /// Put the given key's value in the trie; return the new trie, and the previous value associated with the key, if any
  public func put<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : V) : (Trie<K, V>, ?V) {
      replace(t, k, k_eq, ?v)
    };

  /// Get the value of the given key in the trie, or return null if nonexistent
  public func get<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V =
     find(t, k, k_eq);

  /// Find the given key's value in the trie, or return null if nonexistent
  public func find<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V {
      let key_eq = equalKey(k_eq);
      func rec(t : Trie<K, V>, bitpos : Nat) : ?V {
        switch t {
          case (#empty) { null };
          case (#leaf(l)) {
            AssocList.find(l.keyvals, k, key_eq)
          };
          case (#branch(b)) {
            let bit = Hash.bit(k.hash, bitpos);
            if (not bit) {
              rec(b.left, bitpos + 1)
            }
            else {
              rec(b.right, bitpos + 1)
            }
          };
        }
      };
      rec(t, 0)
    };



  func splitAssocList<K, V>(al : AssocList<Key<K>, V>, bitpos : Nat)
     : (AssocList<Key<K>, V>, AssocList<Key<K>, V>)
   {
     List.partition(
       al,
       func ((k : Key<K>, v : V)) : Bool {
         not Hash.bit(k.hash, bitpos)
       }
     )
   };

  func splitList<K, V>(l : AssocList<Key<K>, V>, bitpos : Nat)
    : (Nat, AssocList<Key<K>, V>, Nat, AssocList<Key<K>, V>)
    {
     func rec(l : AssocList<Key<K>, V>) : (Nat, AssocList<Key<K>, V>, Nat, AssocList<Key<K>, V>) {
         switch l {
           case null { (0, null, 0, null) };
           case (?((k, v), t)) {
             let (cl, l, cr, r) = rec(t) ;
             if (not Hash.bit(k.hash, bitpos)){
               (cl + 1, ?((k, v), l), cr, r)
             } else {
               (cl, l, cr + 1, ?((k, v), r))
             }
           };
         }
       };
     rec(l)
   };

  /// Merge tries, preferring the right trie where there are collisions
  /// in common keys.
  ///
  /// note: the `disj` operation generalizes this `merge`
  /// operation in various ways, and does not (in general) lose
  /// information; this operation is a simpler, special case.
  public func merge<K, V>(tl : Trie<K, V>, tr :  Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V> {
      let key_eq = equalKey(k_eq);
      func rec(bitpos : Nat, tl : Trie<K, V>, tr : Trie<K, V>) : Trie<K, V> {
        switch (tl, tr) {
          case (#empty, _) { return tr };
          case (_, #empty) { return tl };
          case (#leaf(l1), #leaf(l2)) {
            leaf(
              AssocList.disj(
                l1.keyvals, l2.keyvals,
                key_eq,
                func (x : ?V, y : ?V) : V {
                  switch (x, y) {
                    case (null, null) { P.unreachable() };
                    case (null, ?v) { v };
                    case (?v, _) { v };
                  }
                }
              ),
              bitpos)
          };
          case (#leaf(l), _) {
            let (ll, lr) = splitAssocList(l.keyvals, bitpos);
            rec(bitpos, branch(leaf(ll, bitpos), leaf(lr, bitpos)), tr)
          };
          case (_, #leaf(l)) {
            let (ll, lr) = splitAssocList(l.keyvals, bitpos);
            rec(bitpos, tl, branch(leaf(ll, bitpos), leaf(lr, bitpos)))
          };
          case (#branch(b1), #branch(b2)) {
            branch(rec(bitpos + 1, b1.left, b2.left),
                   rec(bitpos + 1, b1.right, b2.right))
          };
        }
      };
      rec(0, tl, tr)
    };

  /// Merge tries like `merge`, except signals a
  /// dynamic error if there are collisions in common keys between the
  /// left and right inputs.
  public func mergeDisjoint<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V> {
      let key_eq = equalKey(k_eq);

      func rec(bitpos : Nat, tl : Trie<K, V>, tr : Trie<K, V>) : Trie<K, V> {
        switch (tl, tr) {
          case (#empty, _) { return tr };
          case (_, #empty) { return tl };
          case (#leaf(l1), #leaf(l2)) {
            leaf(
              AssocList.disjDisjoint(
                l1.keyvals, l2.keyvals,
                func (x : ?V, y : ?V) : V {
                  switch (x, y) {
                    case (null, ?v) { v };
                    case (?v, null) { v };
                    case (_, _) { P.unreachable() };
                  }
                }
              ),
              bitpos
            )
          };
          case (#leaf(l), _) {
            let (ll, lr) = splitAssocList(l.keyvals, bitpos);
            rec(bitpos, branch(leaf(ll, bitpos), leaf(lr, bitpos)), tr)
          };
          case (_, #leaf(l)) {
            let (ll, lr) = splitAssocList(l.keyvals, bitpos);
            rec(bitpos, tl, branch(leaf(ll, bitpos), leaf(lr, bitpos)))
          };
          case (#branch(b1), #branch(b2)) {
            branch(
              rec(bitpos + 1, b1.left, b2.left),
              rec(bitpos + 1, b1.right, b2.right)
            )
          };
        }
      };
      rec(0, tl, tr)
    };

  /// Difference of tries. The output consists are pairs of
  /// the left trie whose keys are not present in the right trie; the
  /// values of the right trie are irrelevant.
  public func diff<K, V, W>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : ( K, K) -> Bool) : Trie<K, V> {
    let key_eq = equalKey(k_eq);

    func rec(bitpos : Nat, tl : Trie<K, V>, tr : Trie<K, W>) : Trie<K, V> {
      switch (tl, tr) {
        case (#empty, _) { return #empty };
        case (_, #empty) { return tl };
        case (#leaf(l1), #leaf(l2)) {
          leaf(
             AssocList.diff(
               l1.keyvals, l2.keyvals,
               key_eq,
             ),
             bitpos
           )
        };
        case (#leaf(l), _) {
          let (ll, lr) = splitAssocList(l.keyvals, bitpos);
          rec(bitpos, branch(leaf(ll, bitpos), leaf(lr, bitpos)), tr)
        };
        case (_, #leaf(l)) {
          let (ll, lr) = splitAssocList(l.keyvals, bitpos);
          rec(bitpos, tl, branch(leaf(ll, bitpos), leaf(lr, bitpos)))
        };
        case (#branch(b1), #branch(b2)) {
          branch(rec(bitpos + 1, b1.left, b2.left),
                 rec(bitpos + 1, b1.right, b2.right))
        };
      }
    };
    rec(0, tl, tr)
  };

  /// Map disjunction.
  ///
  /// This operation generalizes the notion of "set union" to finite maps.
  ///
  /// Produces a "disjunctive image" of the two tries, where the values of
  /// matching keys are combined with the given binary operator.
  ///
  /// For unmatched key-value pairs, the operator is still applied to
  /// create the value in the image.  To accomodate these various
  /// situations, the operator accepts optional values, but is never
  /// applied to (null, null).
  ///
  /// Implements the database idea of an ["outer join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).
  ///
  public func disj<K, V, W, X>(
    tl : Trie<K, V>,
    tr : Trie<K, W>,
    k_eq : (K, K) -> Bool,
    vbin : (?V, ?W) -> X
  ) : Trie<K, X> {
    let key_eq = equalKey(k_eq);

    /* empty right case; build from left only: */
    func recL(t : Trie<K, V>, bitpos : Nat) : Trie<K, X> {
      switch t {
        case (#empty) { #empty };
        case (#leaf(l)) {
          leaf(AssocList.disj(l.keyvals, null, key_eq, vbin), bitpos)
        };
        case (#branch(b)) {
          branch(recL(b.left, bitpos + 1),
                 recL(b.right, bitpos + 1)) };
      }
    };

   /* empty left case; build from right only: */
   func recR(t : Trie<K, W>, bitpos : Nat) : Trie<K, X> {
     switch t {
       case (#empty) { #empty };
       case (#leaf(l)) {
         leaf(AssocList.disj(null, l.keyvals, key_eq, vbin), bitpos)
       };
       case (#branch(b)) {
         branch(recR(b.left, bitpos + 1),
                recR(b.right, bitpos + 1)) };
     }
   };

   /* main recursion */
   func rec(bitpos : Nat, tl : Trie<K, V>, tr : Trie<K, W>) : Trie<K, X> {
     switch (tl, tr) {
       case (#empty, #empty) { #empty };
       case (#empty, _) { recR(tr, bitpos) };
       case (_, #empty) { recL(tl, bitpos) };
       case (#leaf(l1), #leaf(l2)) {
         leaf(AssocList.disj(l1.keyvals, l2.keyvals, key_eq, vbin), bitpos)
       };
       case (#leaf(l), _) {
         let (ll, lr) = splitAssocList(l.keyvals, bitpos);
         rec(bitpos, branch(leaf(ll, bitpos), leaf(lr, bitpos)), tr)
       };
       case (_, #leaf(l)) {
         let (ll, lr) = splitAssocList(l.keyvals, bitpos);
         rec(bitpos, tl, branch(leaf(ll, bitpos), leaf(lr, bitpos)))
       };
       case (#branch(b1), #branch(b2)) {
         branch(rec(bitpos + 1, b1.left, b2.left),
           rec(bitpos + 1, b1.right, b2.right))
       };
      }
    };

    rec(0, tl, tr)
  };

  /// Map join.
  ///
  /// Implements the database idea of an ["inner join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).
  ///
  /// This operation generalizes the notion of "set intersection" to
  /// finite maps.  The values of matching keys are combined with the given binary
  /// operator, and unmatched key-value pairs are not present in the output.
  ///
  public func join<K, V, W, X>(
    tl : Trie<K, V>,
    tr : Trie<K, W>,
    k_eq : (K, K) -> Bool,
    vbin : (V, W) -> X
  ) : Trie<K, X> {
    let key_eq = equalKey(k_eq);

    func rec(bitpos : Nat, tl : Trie<K, V>, tr : Trie<K, W>) : Trie<K, X> {
      switch (tl, tr) {
        case (#empty, _) { #empty };
        case (_, #empty) { #empty };
        case (#leaf(l1), #leaf(l2)) {
          leaf(AssocList.join(l1.keyvals, l2.keyvals, key_eq, vbin), bitpos)
        };
        case (#leaf(l), _) {
          let (ll, lr) = splitAssocList(l.keyvals, bitpos);
          rec(bitpos, branch(leaf(ll, bitpos), leaf(lr, bitpos)), tr)
        };
        case (_, #leaf(l)) {
          let (ll, lr) = splitAssocList(l.keyvals, bitpos);
          rec(bitpos, tl, branch(leaf(ll, bitpos), leaf(lr, bitpos)))
        };
        case (#branch(b1), #branch(b2)) {
          branch(rec(bitpos + 1, b1.left, b2.left),
            rec(bitpos + 1, b1.right, b2.right))
        };
      }
    };

    rec(0, tl, tr)
  };

  /// This operation gives a recursor for the internal structure of
  /// tries.  Many common operations are instantiations of this function,
  /// either as clients, or as hand-specialized versions (e.g., see , map,
  /// mapFilter, some and all below).
  public func foldUp<K, V, X>(t : Trie<K, V>, bin : (X, X) -> X, leaf : (K, V) -> X, empty : X) : X {
    func rec(t : Trie<K, V>) : X {
      switch t {
        case (#empty) { empty };
        case (#leaf(l)) {
          AssocList.fold(
            l.keyvals, empty,
            func (k : Key<K>, v : V, x : X) : X { bin(leaf(k.key, v), x) }
          )
        };
        case (#branch(b)) { bin(rec(b.left), rec(b.right)) };
      }
    };
    rec(t)
  };


  /// Map product.
  ///
  /// Conditional _catesian product_, where the given
  /// operation `op` _conditionally_ creates output elements in the
  /// resulting trie.
  ///
  /// The keyed structure of the input tries are not relevant for this
  /// operation: all pairs are considered, regardless of keys matching or
  /// not.  Moreover, the resulting trie may use keys that are unrelated to
  /// these input keys.
  ///
  public func prod<K1, V1, K2, V2, K3, V3>(
    tl : Trie<K1, V1>,
    tr : Trie<K2, V2>,
    op : (K1, V1, K2, V2) -> ?(Key<K3>, V3),
    k3_eq : (K3, K3) -> Bool
  )  : Trie<K3, V3>  {

    /*- binary case: merge disjoint results: */
    func merge (a : Trie<K3, V3>, b : Trie<K3, V3>) : Trie<K3, V3> =
      mergeDisjoint(a, b, k3_eq);

    /*- "`foldUp` squared" (imagine two nested loops): */
    foldUp(
      tl, merge,
      func (k1 : K1, v1 : V1) : Trie<K3, V3> {
        foldUp(
          tr, merge,
          func (k2 : K2, v2 : V2) : Trie<K3, V3> {
            switch (op(k1, v1, k2, v2)) {
              case null { #empty };
              case (?(k3, v3)) { (put(#empty, k3, k3_eq, v3)).0 };
            }
          },
          #empty
        )
      },
      #empty
    )
  };

  /// Returns an `Iter` over the key-value entries of the trie.
  ///
  /// Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.
  public func iter<K, V>(t : Trie<K, V>) : I.Iter<(K, V)> {
    object {
      var stack = ?(t, null) : List.List<Trie<K, V>>;
      public func next() : ?(K, V) {
        switch stack {
          case null { null };
          case (?(trie, stack2)) {
            switch trie {
              case (#empty) {
                stack := stack2;
                next()
              };
              case (#leaf({ keyvals = null })) {
                stack := stack2;
                next()
              };
              case (#leaf({ size = c; keyvals = ?((k, v), kvs) })) {
                stack := ?(#leaf({ size = c-1; keyvals = kvs }), stack2);
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
    }
  };

  /// Represent the construction of tries as data.
  ///
  /// This module provides optimized variants of normal tries, for
  /// more efficient join queries.
  ///
  /// The central insight is that for (unmaterialized) join query results, we
  /// do not need to actually build any resulting trie of the resulting
  /// data, but rather, just need a collection of what would be in that
  /// trie.  Since query results can be large (quadratic in the DB size),
  /// avoiding the construction of this trie provides a considerable savings.
  ///
  /// To get this savings, we use an ADT for the operations that _would_ build this trie,
  /// if evaluated. This structure specializes a rope: a balanced tree representing a
  /// sequence.  It is only as balanced as the tries from which we generate
  /// these build ASTs.  They have no intrinsic balance properties of their
  /// own.
  ///
  public module Build {
    /// The build of a trie, as an AST for a simple DSL.
    public type Build<K, V> = {
      #skip ;
      #put : (K, ?Hash.Hash, V) ;
      #seq : {
        size : Nat;
        left : Build<K, V>;
        right : Build<K, V>;
      } ;
    };

    /// Size of the build, measured in `#put` operations
    public func size<K, V>(tb : Build<K, V>) : Nat {
      switch tb {
        case (#skip) { 0 };
        case (#put(_, _, _)) { 1 };
        case (#seq(seq)) { seq.size };
      }
    };

    /// Build sequence of two sub-builds
    public func seq<K, V>(l : Build<K, V>, r : Build<K, V>) : Build<K, V> {
        let sum = size(l) + size(r);
        #seq({ size = sum; left = l; right = r })
      };

    /// Like [`prod`](#prod), except do not actually do the put calls, just
    /// record them, as a (binary tree) data structure, isomorphic to the
    /// recursion of this function (which is balanced, in expectation).
    public func prod<K1, V1, K2, V2, K3, V3>(
      tl : Trie<K1, V1>,
      tr : Trie<K2, V2>,
      op : (K1, V1, K2, V2) -> ?(K3, V3),
      k3_eq : (K3, K3) -> Bool
    ) : Build<K3, V3> {

      func outer_bin (a : Build<K3, V3>, b : Build<K3, V3>)
        : Build<K3, V3> {
          seq(a, b)
        };

      func inner_bin (a : Build<K3, V3>, b : Build<K3, V3>)
        : Build<K3, V3> {
          seq(a, b)
        };

      /// double-nested folds
      foldUp(
        tl, outer_bin,
        func (k1 : K1, v1 : V1) : Build<K3, V3> {
          foldUp(
            tr, inner_bin,
            func (k2 : K2, v2 : V2) : Build<K3, V3> {
              switch (op(k1, v1, k2, v2)) {
                case null { #skip };
                case (?(k3, v3)) { #put(k3, null, v3) };
              }
            },
            #skip
          )
        },
        #skip
      )
    };

    /// Project the nth key-value pair from the trie build.
    ///
    /// This position is meaningful only when the build contains multiple uses of one or more keys, otherwise it is not.
    public func nth<K, V>(tb : Build<K, V>, i : Nat) : ?(K, ?Hash.Hash, V) {
        func rec(tb : Build<K, V>, i : Nat) : ?(K, ?Hash.Hash, V) {
          switch tb {
            case (#skip) { P.unreachable() };
            case (#put(k, h, v)) {
              assert(i == 0);
              ?(k, h, v)
             };
             case (#seq(s)) {
               let size_left = size(s.left);
               if (i < size_left) { rec(s.left,  i) }
               else { rec(s.right, i - size_left) }
             };
          }
        };

        if (i >= size(tb)) {
          return null
        };
        rec(tb, i)
      };

    /// Like [`mergeDisjoint`](#mergedisjoint), except that it avoids the
    /// work of actually merging any tries; rather, just record the work for
    /// latter (if ever).
    public func projectInner<K1, K2, V>(t : Trie<K1, Build<K2, V>>)
      : Build<K2, V> {
      foldUp(
        t,
        func (t1 : Build<K2, V>, t2 : Build<K2, V>) : Build<K2, V> { seq(t1, t2) },
        func (_ : K1, t : Build<K2, V>) : Build<K2, V> { t },
        #skip
      )
    };

    /// Gather the collection of key-value pairs into an array of a (possibly-distinct) type.
    public func toArray<K, V, W>(tb : Build<K, V>, f: (K, V) -> W) : [W] {
      let c = size(tb);
      let a = A.init<?W>(c, null);
      var i = 0;
      func rec(tb : Build<K, V>) {
        switch tb {
          case (#skip) {};
          case (#put(k, _, v)) { a[i] := ?f(k, v); i := i + 1 };
          case (#seq(s)) { rec(s.left); rec(s.right) };
        }
      };
      rec(tb);
      A.tabulate(c, func(i : Nat) : W {
        switch (a[i]) {
          case null { P.unreachable() };
          case (?x) { x }
        }})
    };

  };

  /// Fold over the key-value pairs of the trie, using an accumulator.
  /// The key-value pairs have no reliable or meaningful ordering.
  public func fold<K, V, X>(t : Trie<K, V>, f : (K, V, X) -> X, x : X) : X {
    func rec(t : Trie<K, V>, x : X) : X {
      switch t {
        case (#empty) { x };
        case (#leaf(l)) {
          AssocList.fold(
           l.keyvals, x,
           func (k : Key<K>, v : V, x : X) : X = f(k.key, v, x)
          )
        };
        case (#branch(b)) { rec(b.left, rec(b.right, x)) };
      };
    };
    rec(t, x)
  };

  /// Test whether a given key-value pair is present, or not.
  public func some<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool {
    func rec(t : Trie<K, V>) : Bool {
      switch t {
        case (#empty) { false };
        case (#leaf(l)) {
          List.some(
            l.keyvals, func ((k : Key<K>, v:V)) : Bool = f(k.key, v)
          )
        };
        case (#branch(b)) { rec(b.left) or rec(b.right) };
      };
    };
    rec(t)
  };

  /// Test whether all key-value pairs have a given property.
  public func all<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool {
    func rec(t : Trie<K, V>) : Bool {
      switch t {
        case (#empty) { true };
        case (#leaf(l)) {
          List.all(
            l.keyvals, func ((k : Key<K>, v : V)) : Bool=f(k.key, v)
          )
        };
        case (#branch(b)) { rec(b.left) and rec(b.right) };
      };
    };
    rec(t)
  };

  /// Project the nth key-value pair from the trie.
  ///
  /// Note: This position is not meaningful; it's only here so that we
  /// can inject tries into arrays using functions like `Array.tabulate`.
  public func nth<K, V>(t : Trie<K, V>, i : Nat) : ?(Key<K>, V) {
      func rec(t : Trie<K, V>, i : Nat) : ?(Key<K>, V) {
        switch t {
          case (#empty) { P.unreachable() };
          case (#leaf(l)) { List.get(l.keyvals, i) };
          case (#branch(b)) {
            let size_left = size(b.left);
            if (i < size_left) { rec(b.left,  i) }
            else { rec(b.right, i - size_left) }
          }
        }
      };
      if (i >= size(t)) {
        return null
      };
      rec(t, i)
  };


  /// Gather the collection of key-value pairs into an array of a (possibly-distinct) type.
  public func toArray<K, V, W>(t : Trie<K, V>, f : (K, V) -> W) : [W] {
      let a = A.tabulate<W> (
        size(t),
        func (i : Nat) : W {
          let (k, v) = switch (nth(t, i)) {
            case null { P.unreachable() };
            case (?x) { x };
          };
          f(k.key, v)
        }
      );
      a
    };

  /// Test for "deep emptiness": subtrees that have branching structure,
  /// but no leaves.  These can result from naive filtering operations;
  /// filter uses this function to avoid creating such subtrees.
  public func isEmpty<K, V>(t : Trie<K, V>) : Bool {
    size(t) == 0
  };

  /// Filter the key-value pairs by a given predicate.
  public func filter<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Trie<K, V> {
    func rec(t : Trie<K, V>, bitpos : Nat) : Trie<K, V> {
      switch t {
        case (#empty) { #empty };
        case (#leaf(l)) {
          leaf(
            List.filter(
              l.keyvals,
              func ((k : Key<K>, v : V)) : Bool = f(k.key, v)
            ),
            bitpos
          )
        };
        case (#branch(b)) {
          let fl = rec(b.left, bitpos + 1);
          let fr = rec(b.right, bitpos + 1);
          if (isEmpty(fl) and isEmpty(fr)) {
            #empty
          } else {
            branch(fl, fr)
          }
        }
      }
    };
    rec(t, 0)
  };

  /// Map and filter the key-value pairs by a given predicate.
  public func mapFilter<K, V, W>(t : Trie<K, V>, f : (K, V) -> ?W) : Trie<K, W> {
    func rec(t : Trie<K, V>, bitpos : Nat) : Trie<K, W> {
      switch t {
        case (#empty) { #empty };
        case (#leaf(l)) {
          leaf(
            List.mapFilter(
              l.keyvals,
              // retain key and hash, but update key's value using f:
              func ((k : Key<K>, v : V)) : ?(Key<K>, W) {
                switch (f(k.key, v)) {
                  case null { null };
                  case (?w) { ?({key = k.key; hash = k.hash}, w) };
                }
              }
            ),
            bitpos
          )
        };
        case (#branch(b)) {
          let fl = rec(b.left, bitpos + 1);
          let fr = rec(b.right, bitpos + 1);
          if (isEmpty(fl) and isEmpty(fr)) {
            #empty
          } else {
            branch(fl, fr)
          }
        }
      }
    };

   rec(t, 0)
  };

  /// Test for equality, but naively, based on structure.
  /// Does not attempt to remove "junk" in the tree;
  /// For instance, a "smarter" approach would equate
  ///   `#bin {left = #empty; right = #empty}`
  /// with
  ///   `#empty`.
  /// We do not observe that equality here.
  public func equalStructure<K, V>(
    tl : Trie<K, V>,
    tr : Trie<K, V>,
    keq : (K, K) -> Bool,
    veq : (V, V) -> Bool
  ) : Bool {
    func rec(tl : Trie<K, V>, tr : Trie<K, V>) : Bool {
      switch (tl, tr) {
        case (#empty, #empty) { true };
        case (#leaf(l1), #leaf(l2)) {
          List.equal(l1.keyvals, l2.keyvals,
            func ((k1 : Key<K>, v1 : V), (k2 : Key<K>, v2 : V)) : Bool =
              keq(k1.key, k2.key) and veq(v1, v2)
          )
        };
        case (#branch(b1), #branch(b2)) {
          rec(b1.left, b2.left) and rec(b2.right, b2.right)
        };
        case _ { false };
      }
    };
    rec(tl, tr)
  };

  /// Replace the given key's value in the trie,
  /// and only if successful, do the success continuation,
  /// otherwise, return the failure value
  public func replaceThen<K, V, X>(
    t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v2 : V,
    success: (Trie<K, V>, V) -> X,
    fail: () -> X
  ) : X {
    let (t2, ov) = replace(t, k, k_eq, ?v2);
    switch ov {
      case null { /* no prior value; failure to remove */ fail() };
      case (?v1) { success(t2, v1) };
    }
  };

  /// Put the given key's value in the trie; return the new trie; assert that no prior value is associated with the key
  public func putFresh<K, V>(t : Trie<K, V>,  k : Key<K>, k_eq : (K, K) -> Bool, v : V) : Trie<K, V> {
    let (t2, none) = replace(t, k, k_eq, ?v);
    switch none {
      case null {};
      case (?_) assert false;
    };
    t2
  };

  /// Put the given key's value in the 2D trie; return the new 2D trie.
  public func put2D<K1, K2, V>(
    t : Trie2D<K1, K2, V>,
    k1 : Key<K1>,
    k1_eq : (K1, K1) -> Bool,
    k2 : Key<K2>,
    k2_eq : (K2, K2) -> Bool,
    v:V
  ) : Trie2D<K1, K2, V> {
    let inner = find(t, k1, k1_eq);
    let (updated_inner, _) = switch inner {
      case null { put(#empty, k2, k2_eq, v) };
      case (?inner) { put(inner, k2, k2_eq, v) };
    };
    let (updated_outer, _) = put(t, k1, k1_eq, updated_inner);
    updated_outer;
  };

  /// Put the given key's value in the trie; return the new trie;
  public func put3D<K1, K2, K3, V> (
    t : Trie3D<K1, K2, K3, V>,
    k1 : Key<K1>,
    k1_eq : (K1, K1) -> Bool,
    k2 : Key<K2>,
    k2_eq : (K2, K2) -> Bool,
    k3 : Key<K3>,
    k3_eq : (K3, K3) -> Bool,
    v : V
  ) : Trie3D<K1, K2, K3, V> {
    let inner1 = find(t, k1, k1_eq);
    let (updated_inner1, _) = switch inner1 {
      case null {
        put(
          #empty, k2, k2_eq,
           (put(#empty, k3, k3_eq, v)).0
        )
      };
      case (?inner1) {
        let inner2 = find(inner1, k2, k2_eq);
        let (updated_inner2, _) = switch inner2 {
          case null { put(#empty, k3, k3_eq, v) };
          case (?inner2) { put(inner2, k3, k3_eq, v) };
        };
        put(inner1, k2, k2_eq, updated_inner2 )
      };
    };
    let (updated_outer, _) = put(t, k1, k1_eq, updated_inner1);
    updated_outer;
  };

  /// Remove the given key's value in the trie; return the new trie
  public func remove<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : (Trie<K, V>, ?V) {
    replace(t, k, k_eq, null)
  };

  /// Remove the given key's value in the trie,
  /// and only if successful, do the success continuation,
  /// otherwise, return the failure value
  public func removeThen<K, V, X>(
    t : Trie<K, V>,
    k : Key<K>,
    k_eq : (K, K) -> Bool,
    success : (Trie<K, V>, V) -> X,
    fail : () -> X
  ) : X {
    let (t2, ov) = replace(t, k, k_eq, null);
    switch ov {
      case null { /* no prior value; failure to remove */ fail() };
      case (?v) { success(t2, v) };
    }
  };


  /// remove the given key-key pair's value in the 2D trie; return the
  /// new trie, and the prior value, if any.
  public func remove2D<K1, K2, V>(
    t : Trie2D<K1, K2, V>,
    k1 : Key<K1>,
    k1_eq : (K1, K1) -> Bool,
    k2 : Key<K2>,
    k2_eq : (K2, K2) -> Bool
  ) : (Trie2D<K1, K2, V>, ?V)  {
    switch (find(t, k1, k1_eq)) {
      case null {
        (t, null)
      };
      case (?inner) {
        let (updated_inner, ov) = remove(inner, k2, k2_eq);
        let (updated_outer, _) = put(t, k1, k1_eq, updated_inner);
        (updated_outer, ov)
      };
    }
  };

  /// Remove the given key-key pair's value in the 3D trie; return the
  /// new trie, and the prior value, if any.
  public func remove3D<K1, K2, K3, V>(
    t : Trie3D<K1, K2, K3, V>,
    k1 : Key<K1>,
    k1_eq : (K1, K1) -> Bool,
    k2 : Key<K2>,
    k2_eq : (K2, K2) -> Bool,
    k3 : Key<K3>,
    k3_eq : (K3, K3) -> Bool,
  ) : (Trie3D<K1, K2, K3, V>, ?V) {
    switch (find(t, k1, k1_eq)) {
      case null {
        (t, null)
      };
      case (?inner) {
        let (updated_inner, ov) = remove2D(inner, k2, k2_eq, k3, k3_eq);
        let (updated_outer, _) = put(t, k1, k1_eq, updated_inner);
        (updated_outer, ov)
      };
    }
  };

  /// Like [`mergeDisjoint`](#mergedisjoint), except instead of merging a
  /// pair, it merges the collection of dimension-2 sub-trees of a 2D
  /// trie.
  public func mergeDisjoint2D<K1, K2, V>(
    t : Trie2D<K1, K2, V>,
    k1_eq : (K1, K1) -> Bool,
    k2_eq : (K2, K2) -> Bool
  ) : Trie<K2, V> {
    foldUp(
      t,
      func (t1 : Trie<K2, V>, t2 : Trie<K2, V>) : Trie<K2, V> {
        mergeDisjoint(t1, t2, k2_eq)
      },
      func (_ : K1, t : Trie<K2, V>) : Trie<K2, V> { t },
      #empty
    )
  };

}
