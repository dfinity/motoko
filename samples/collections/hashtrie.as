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
//  - write trie operations that operate over pairs of tries:
//    for set union, difference and intersection.

// TODO-Matthew:
//
//  - (more) regression tests for everything that is below
//
//  - handle hash collisions gracefully;
//    ==> Blocked on AS module support, for using List module.
//
//  - adapt the path length of each subtree to its cardinality; avoid
//    needlessly long paths, or paths that are too short for their
//    subtree's size.
//
//  - iterator objects, for use in 'for ... in ...' patterns


// import List

// TEMP: A "bit string" as a linked list of bits:
type Bits = ?(Bool, Bits);

// TODO: Replace this definition WordX, for some X, once we have these types in AS.
type Hash = Bits;
//type Hash = Word16;
//type Hash = Word8;

// Uniform depth assumption:
//
// We make a simplifying assumption, for now: All defined paths in the
// trie have a uniform length, the same as the number of bits of a
// hash, starting at the LSB, that we use for indexing.
//
// - If the number is too low, our expected O(log n) bounds become
//   expected O(n).
//
// - If the number is too high, we waste constant factors for
//   representing small sets/maps.
//
// TODO: Make this more robust by making this number adaptive for each
// path, and based on the content of the trie along that path.
//
let HASH_BITS = 4;

// XXX: See AST-42
type Node<K,V> = {
  left:Trie<K,V>;
  right:Trie<K,V>;
  key:?K;
  val:?V
};
type Trie<K,V> = ?Node<K,V>;

/* See AST-42 (sum types); we want this type definition instead:

// Use a sum type (AST-42)
type Trie<K,V>     = { #leaf : LeafNode<K,V>; #bin : BinNode<K,V>; #empty };
type BinNode<K,V>  = { left:Trie<K,V>; right:Trie<K,V> };
type LeafNode<K,V> = { key:K; val:V };

*/

// XXX: until AST-42:
func Trie__isNull<X>(x : ?X) : Bool {
  switch x {
    case null { true  };
    case (?_) { false };
  };
};

// XXX: until AST-42:
func Trie__assertIsNull<X>(x : ?X) {
  switch x {
    case null { assert(true)  };
    case (?_) { assert(false) };
  };
};

// XXX: until AST-42:
func Trie__makeEmpty<K,V>() : Trie<K,V>
  = null;

// Note: More general version of this operation below, which tests for
// "deep emptiness" (subtrees that have branching structure, but no
// leaves; these can result from naive filtering operations, for
// instance).
//
// // XXX: until AST-42:
// func isEmpty<K,V>(t:Trie<K,V>) : Bool {
//   switch t {
//     case null { true  };
//     case (?_) { false };
//   };
// };

// XXX: until AST-42:
func Trie__assertIsEmpty<K,V>(t : Trie<K,V>) {
  switch t {
    case null { assert(true)  };
    case (?_) { assert(false) };
  };
};

// XXX: until AST-42:
func Trie__makeBin<K,V>(l:Trie<K,V>, r:Trie<K,V>) : Trie<K,V>  {
  ?(new {left=l; right=r; key=null; val=null })
};

// XXX: until AST-42:
func Trie__isBin<K,V>(t:Trie<K,V>) : Bool {
  switch t {
  case null { false };
  case (?t_) {
         switch (t_.key) {
         case null { true };
         case _ { false };
         };
       };
  }
};

// XXX: until AST-42:
func Trie__makeLeaf<K,V>(k:K, v:V) : Trie<K,V> {
  ?(new {left=null; right=null; key=?k; val=?v })
};

// XXX: until AST-42:
func Trie__matchLeaf<K,V>(t:Trie<K,V>) : ?(K,V) {
  switch t {
  case null { null };
  case (?t_) {
         switch (t_.key, t_.val) {
         case (?k,?v) ?(k,v);
         case (_)     null;
         }
       };
  }
};

// XXX: until AST-42:
func Trie__isLeaf<K,V>(t:Trie<K,V>) : Bool {
  switch t {
  case null { false };
  case (?t_) {
         switch (t_.key) {
         case null { false };
         case _ { true };
         }
       };
  }
};
// XXX: until AST-42:
func Trie__assertIsBin<K,V>(t : Trie<K,V>) {
  switch t {
    case null { assert(false) };
    case (?n) {
      Trie__assertIsNull<K>(n.key);
      Trie__assertIsNull<V>(n.val);
   };
  }
};

// XXX: until AST-42:
func Trie__getLeafKey<K,V>(t : Node<K,V>) : K {
  Trie__assertIsNull<Node<K,V>>(t.left);
  Trie__assertIsNull<Node<K,V>>(t.right);
  switch (t.key) {
    case (?k) { k };
    case null { Trie__getLeafKey<K,V>(t) };
  }
};

// XXX: this helper is an ugly hack; we need real sum types to avoid it, I think:
func Trie__getLeafVal<K,V>(t : Node<K,V>) : ?V {
  Trie__assertIsNull<Node<K,V>>(t.left);
  Trie__assertIsNull<Node<K,V>>(t.right);
  t.val
};

// TODO: Replace with bitwise operations on Words, once we have each of those in AS.
// For now, we encode hashes as lists of booleans.
func Trie__getHashBit(h:Hash, pos:Nat) : Bool {
  switch h {
  case null {
         // XXX: Should be an error case; it shouldn't happen in our tests if we set them up right.
         false
       };
  case (?(b, h_)) {
         if (pos == 0) { b }
         else { Trie__getHashBit(h_, pos-1) }
       };
  }
};

// part of "public interface":
func Trie__empty<K,V>() : Trie<K,V> = Trie__makeEmpty<K,V>();

// helper function for constructing new paths of uniform length
func Trie__buildNewPath<K,V>(bitpos:Nat, k:K, k_hash:Hash, ov:?V) : Trie<K,V> {
  func rec(bitpos:Nat) : Trie<K,V> {
    if ( bitpos < HASH_BITS ) {
      // create new bin node for this bit of the hash
      let path = rec(bitpos+1);
      let bit = Trie__getHashBit(k_hash, bitpos);
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
func Trie__replace<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, v:?V) : (Trie<K,V>, ?V) {
  // For `bitpos` in 0..HASH_BITS, walk the given trie and locate the given value `x`, if it exists.
  func rec(t : Trie<K,V>, bitpos:Nat) : (Trie<K,V>, ?V) {
    if ( bitpos < HASH_BITS ) {
      switch t {
      case null { (Trie__buildNewPath<K,V>(bitpos, k, k_hash, v), null) };
      case (?n) {
        Trie__assertIsBin<K,V>(t);
        let bit = Trie__getHashBit(k_hash, bitpos);
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
        case null { (Trie__buildNewPath<K,V>(bitpos, k, k_hash, v), null) };
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
func Trie__insert<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, v:V) : (Trie<K,V>, ?V) {
  Trie__replace<K,V>(t, k, k_hash, ?v)
};

// remove the given key's value in the trie; return the new trie
func Trie__remove<K,V>(t : Trie<K,V>, k:K, k_hash:Hash) : (Trie<K,V>, ?V) {
  Trie__replace<K,V>(t, k, k_hash, null)
};

// find the given key's value in the trie, or return null if nonexistent
func Trie__find<K,V>(t : Trie<K,V>, k:K, k_hash:Hash, keq:(K,K) -> Bool) : ?V {
  // For `bitpos` in 0..HASH_BITS, walk the given trie and locate the given value `x`, if it exists.
  func rec(t : Trie<K,V>, bitpos:Nat) : ?V {
    if ( bitpos < HASH_BITS ) {
      switch t {
      case null {
        // the trie may be "sparse" along paths leading to no keys, and may end early.
        null
      };
      case (?n) {
        Trie__assertIsBin<K,V>(t);
        let bit = Trie__getHashBit(k_hash, bitpos);
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
           if (keq(Trie__getLeafKey<K,V>(l), k)) {
             Trie__getLeafVal<K,V>(l)
           } else {
             null
           }
        };
      }
    }
  };
  rec(t, 0)
};

// merge tries, preferring the right trie where there are collisions
// in common keys. note: the `disj` operation generalizes this `merge`
// operation in various ways, and does not (in general) loose
// information; this operation is a simpler, special case.
func Trie__merge<K,V>(tl:Trie<K,V>, tr:Trie<K,V>) : Trie<K,V> {
  switch (tl, tr) {
    case (null, _) { return tr };
    case (_, null) { return tl };
    case (?nl,?nr) {
    switch (Trie__isBin<K,V>(tl),
            Trie__isBin<K,V>(tr)) {
    case (true, true) {
           let t0 = Trie__merge<K,V>(nl.left, nr.left);
           let t1 = Trie__merge<K,V>(nl.right, nr.right);
           Trie__makeBin<K,V>(t0, t1)
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
           /// XXX: handle hash collisions here.
           tr
         };
      }
   };
  }
};

// The key-value pairs of the final trie consists of those pairs of
// the left trie whose keys are not present in the right trie; the
// values of the right trie are irrelevant.
func Trie__diff<K,V,W>(tl:Trie<K,V>, tr:Trie<K,W>, keq:(K,K)->Bool) : Trie<K,V> {
  func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,V> {
    switch (tl, tr) {
    case (null, _) { return Trie__makeEmpty<K,V>() };
    case (_, null) { return tl };
    case (?nl,?nr) {
    switch (Trie__isBin<K,V>(tl),
            Trie__isBin<K,W>(tr)) {
    case (true, true) {
           let t0 = rec(nl.left, nr.left);
           let t1 = rec(nl.right, nr.right);
           Trie__makeBin<K,V>(t0, t1)
         };
    case (false, true) {
           assert(false);
           // XXX impossible, until we lift uniform depth assumption
           tl
         };
    case (true, false) {
           assert(false);
           // XXX impossible, until we lift uniform depth assumption
           tl
         };
    case (false, false) {
           /// XXX: handle hash collisions here.
           switch (nl.key, nr.key) {
             case (?kl, ?kr) {
               if (keq(kl, kr)) {
                 Trie__makeEmpty<K,V>();
               } else {
                 tl
               }};
             // XXX impossible, and unnecessary with AST-42.
             case _ { tl }
           }
         };
      }
   };
  }};
  rec(tl, tr)
};

// This operation generalizes the notion of "set union" to finite maps.
// Produces a "disjunctive image" of the two tries, where the values of
// matching keys are combined with the given binary operator.
//
// For unmatched key-value pairs, the operator is still applied to
// create the value in the image.  To accomodate these various
// situations, the operator accepts optional values, but is never
// applied to (null, null).
//
func Trie__disj<K,V,W,X>(tl:Trie<K,V>, tr:Trie<K,W>,
                         keq:(K,K)->Bool, vbin:(?V,?W)->X)
  : Trie<K,X>
{
  func recL(t:Trie<K,V>) : Trie<K,X> {
    switch t {
      case (null) null;
      case (? n) {
      switch (Trie__matchLeaf<K,V>(t)) {
        case (?(k,v)) { Trie__makeLeaf<K,X>(k, vbin(?v, null)) };
        case _ { Trie__makeBin<K,X>(recL(n.left), recL(n.right)) }
      }
    };
  }};
  func recR(t:Trie<K,W>) : Trie<K,X> {
    switch t {
      case (null) null;
      case (? n) {
      switch (Trie__matchLeaf<K,W>(t)) {
        case (?(k,w)) { Trie__makeLeaf<K,X>(k, vbin(null, ?w)) };
        case _ { Trie__makeBin<K,X>(recR(n.left), recR(n.right)) }
      }
    };
  }};
  func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
    switch (tl, tr) {
    // empty-empty terminates early, all other cases do not.
    case (null, null) { Trie__makeEmpty<K,X>() };
    case (null, _   ) { recR(tr) };
    case (_,    null) { recL(tl) };
    case (? nl, ? nr) {
    switch (Trie__isBin<K,V>(tl),
            Trie__isBin<K,W>(tr)) {
    case (true, true) {
           let t0 = rec(nl.left, nr.left);
           let t1 = rec(nl.right, nr.right);
           Trie__makeBin<K,X>(t0, t1)
         };
    case (false, true) {
           assert(false);
           // XXX impossible, until we lift uniform depth assumption
           Trie__makeEmpty<K,X>()
         };
    case (true, false) {
           assert(false);
           // XXX impossible, until we lift uniform depth assumption
           Trie__makeEmpty<K,X>()
         };
    case (false, false) {
           assert(Trie__isLeaf<K,V>(tl));
           assert(Trie__isLeaf<K,W>(tr));
           switch (nl.key, nl.val, nr.key, nr.val) {
             // leaf-leaf case
             case (?kl, ?vl, ?kr, ?vr) {
               if (keq(kl, kr)) {
                 Trie__makeLeaf<K,X>(kl, vbin(?vl, ?vr));
               } else {
                 // XXX: handle hash collisions here.
                 Trie__makeEmpty<K,X>()
               }
             };
             // XXX impossible, and unnecessary with AST-42.
             case _ { Trie__makeEmpty<K,X>() };
           }
         };
      }
   };
  }};
  rec(tl, tr)
};

// This operation generalizes the notion of "set intersection" to
// finite maps.  Produces a "conjuctive image" of the two tries, where
// the values of matching keys are combined with the given binary
// operator, and unmatched key-value pairs are not present in the output.
func Trie__conj<K,V,W,X>(tl:Trie<K,V>, tr:Trie<K,W>,
                   keq:(K,K)->Bool, vbin:(V,W)->X)
  : Trie<K,X>
{
  func rec(tl:Trie<K,V>, tr:Trie<K,W>) : Trie<K,X> {
    switch (tl, tr) {
      case (null, null) { return Trie__makeEmpty<K,X>() };
      case (null, ? nr) { return Trie__makeEmpty<K,X>() };
      case (? nl, null) { return Trie__makeEmpty<K,X>() };
      case (? nl, ? nr) {
      switch (Trie__isBin<K,V>(tl),
              Trie__isBin<K,W>(tr)) {
      case (true, true) {
             let t0 = rec(nl.left, nr.left);
             let t1 = rec(nl.right, nr.right);
             Trie__makeBin<K,X>(t0, t1)
           };
      case (false, true) {
             assert(false);
             // XXX impossible, until we lift uniform depth assumption
             Trie__makeEmpty<K,X>()
           };
      case (true, false) {
             assert(false);
             // XXX impossible, until we lift uniform depth assumption
             Trie__makeEmpty<K,X>()
           };
      case (false, false) {
             assert(Trie__isLeaf<K,V>(tl));
             assert(Trie__isLeaf<K,W>(tr));
             switch (nl.key, nl.val, nr.key, nr.val) {
               // leaf-leaf case
             case (?kl, ?vl, ?kr, ?vr) {
                    if (keq(kl, kr)) {
                      Trie__makeLeaf<K,X>(kl, vbin(vl, vr));
                    } else {
                      // XXX: handle hash collisions here.
                      Trie__makeEmpty<K,X>()
                    }
                  };
             // XXX impossible, and unnecessary with AST-42.
             case _ { Trie__makeEmpty<K,X>() };
             }
           };
          }
         }
    }};
  rec(tl, tr)
};

// This operation gives a recursor for the internal structure of
// tries.  Many common operations are instantiations of this function,
// either as clients, or as hand-specialized versions (e.g., see map,
// mapFilter, exists and forAll below).
func Trie__foldUp<K,V,X>(t:Trie<K,V>, bin:(X,X)->X, leaf:(K,V)->X, empty:X) : X {
  func rec(t:Trie<K,V>) : X {
    switch t {
    case (null) { empty };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) { leaf(k,v) };
           case null { bin(rec(n.left), rec(n.right)) };
           }
    };
    }};
  rec(t)
};

// Fold over the key-value pairs of the trie, using an accumulator.
// The key-value pairs have no reliable or meaningful ordering.
func Trie__fold<K,V,X>(t:Trie<K,V>, f:(K,V,X)->X, x:X) : X {
  func rec(t:Trie<K,V>, x:X) : X {
    switch t {
    case (null) x;
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) { f(k,v,x) };
           case null { rec(n.left,rec(n.right,x)) };
           }
    };
    }};
  rec(t, x)
};

// specialized foldUp operation.
func Trie__exists<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Bool {
  func rec(t:Trie<K,V>) : Bool {
    switch t {
    case (null) { false };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) { f(k,v) };
           case null { rec(n.left) or rec(n.right) };
           }
    };
    }};
  rec(t)
};

// specialized foldUp operation.
func Trie__forAll<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Bool {
  func rec(t:Trie<K,V>) : Bool {
    switch t {
    case (null) { true };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) { f(k,v) };
           case null { rec(n.left) and rec(n.right) };
           }
    };
    }};
  rec(t)
};

// specialized foldUp operation.
// Test for "deep emptiness": subtrees that have branching structure,
// but no leaves.  These can result from naive filtering operations;
// filter uses this function to avoid creating such subtrees.
func Trie__isEmpty<K,V>(t:Trie<K,V>) : Bool {
  func rec(t:Trie<K,V>) : Bool {
    switch t {
    case (null) { true };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) { false };
           case null { rec(n.left) and rec(n.right) };
           }
         };
    }
  };
  rec(t)
};

func Trie__filter<K,V>(t:Trie<K,V>, f:(K,V)->Bool) : Trie<K,V> {
  func rec(t:Trie<K,V>) : Trie<K,V> {
    switch t {
    case (null) { null };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) {
                  // XXX-Typechecker:
                  //  This version of the next line gives _really_
                  //  strange type errors, and no parse errors.
                  // if f(k,v) {
                  if (f(k,v)) {
                    Trie__makeLeaf<K,V>(k,v)
                  } else {
                    null
                  }
                };
           case null {
                  let l = rec(n.left);
                  let r = rec(n.right);
                  switch (Trie__isEmpty<K,V>(l),
                          Trie__isEmpty<K,V>(r)) {
                    case (true,  true)  null;
                    case (false, true)  r;
                    case (true,  false) l;
                    case (false, false) Trie__makeBin<K,V>(l, r);
                  }
                };
           }
         };
    }
  };
  rec(t)
};

func Trie__mapFilter<K,V,W>(t:Trie<K,V>, f:(K,V)->?(K,W)) : Trie<K,W> {
  func rec(t:Trie<K,V>) : Trie<K,W> {
    switch t {
    case (null) { null };
    case (?n) {
           switch (Trie__matchLeaf<K,V>(t)) {
           case (?(k,v)) {
                  switch (f(k,v)) {
                    case (null) null;
                    case (?(k,w)) { Trie__makeLeaf<K,W>(k,w) };
                }};
           case null {
                  let l = rec(n.left);
                  let r = rec(n.right);
                  switch (Trie__isEmpty<K,W>(l),
                          Trie__isEmpty<K,W>(r)) {
                    case (true,  true)  null;
                    case (false, true)  r;
                    case (true,  false) l;
                    case (false, false) Trie__makeBin<K,W>(l, r);
                  }
                };
           }
         };
    }
  };
  rec(t)
};

// Test for equality, but naively, based on structure.
// Does not attempt to remove "junk" in the tree;
// For instance, a "smarter" approach would equate
//   `#bin{left=#empty;right=#empty}`
// with
//   `#empty`.
// We do not observe that equality here.
func Trie__equalStructure<K,V>(
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
           switch (Trie__matchLeaf<K,V>(tl),
                   Trie__matchLeaf<K,V>(tr)) {
           case (?(kl,vl), ?(kr,vr)) { keq(kl,kr) and veq(vl,vr) };
           case (null,     null)     { rec(nl.left, nr.left)
                                       and rec(nl.right, nr.right) };
           case _ { false }
           }
    };
    }};
  rec(tl, tr)
};

// Create a record,
// as a standin until we have "real" modules to create namespaces:
let Trie = new {
  // Meta-level stuff:
  // ---------------------
  moduleName = "Trie"

  ; empty          = Trie__empty
  ; insert         = Trie__insert
  ; remove         = Trie__remove
  ; find           = Trie__find
  ; replace        = Trie__replace
  ; merge          = Trie__merge
  ; diff           = Trie__diff
  ; disj           = Trie__disj
  ; conj           = Trie__conj
  ; foldUp         = Trie__foldUp
  ; fold           = Trie__fold
  ; exists         = Trie__exists
  ; forAll         = Trie__forAll
  ; isEmpty        = Trie__isEmpty
  ; filter         = Trie__filter
  ; mapFilter      = Trie__mapFilter
  ; equalStructure = Trie__equalStructure
};
