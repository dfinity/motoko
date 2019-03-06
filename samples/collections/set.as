// import Trie;

///////////////////////////////////////////////////////////////////////

/*
 Sets are partial maps from element type to unit type,
 i.e., the partial map represents the set with its domain.
*/

// TODO-Matthew:
//
// - for now, we pass a hash value each time we pass an element value;
//   in the future, we might avoid passing element hashes with each element in the API;
//   related to: https://dfinity.atlassian.net/browse/AST-32
//
// - similarly, we pass an equality function when we do some operations.
//   in the future, we might avoid this via https://dfinity.atlassian.net/browse/AST-32
//

type Set<T> = Trie<T,()>;

func Set__empty<T>():Set<T> =
  Trie.empty<T,()>();

func Set__insert<T>(s:Set<T>, x:T, xh:Hash):Set<T> = {
  let (s2, _) = Trie.insert<T,()>(s, x, xh, ());
  s2
};

func Set__remove<T>(s:Set<T>, x:T, xh:Hash):Set<T> = {
  let (s2, _) = Trie.remove<T,()>(s, x, xh);
  s2
};

func Set__eq<T>(s1:Set<T>, s2:Set<T>, eq:(T,T)->Bool):Bool {
  // XXX: Todo: use a smarter check
  Trie.equalStructure<T,()>(s1, s2, eq, Set__unitEq)
};

func Set__card<T>(s:Set<T>) : Nat {
  Trie.foldUp<T,(),Nat>
  (s,
   func(n:Nat,m:Nat):Nat{n+m},
   func(_:T,_:()):Nat{1},
   0)
};

func Set__mem<T>(s:Set<T>, x:T, xh:Hash, eq:(T,T)->Bool):Bool {
  switch (Trie.find<T,()>(s, x, xh, eq)) {
  case null { false };
  case (?_) { true };
  }
};

func Set__union<T>(s1:Set<T>, s2:Set<T>):Set<T> {
  let s3 = Trie.merge<T,()>(s1, s2);
  s3
};

func Set__diff<T>(s1:Set<T>, s2:Set<T>, eq:(T,T)->Bool):Set<T> {
  let s3 = Trie.diff<T,(),()>(s1, s2, eq);
  s3
};

func Set__intersect<T>(s1:Set<T>, s2:Set<T>, eq:(T,T)->Bool):Set<T> {
  let noop : ((),())->(()) = func (_:(),_:()):(())=();
  let s3 = Trie.conj<T,(),(),()>(s1, s2, eq, noop);
  s3
};

func Set__unitEq (_:(),_:()):Bool{ true };

// Create a record,
// as a standin until we have "real" modules to create namespaces:
let Set = new {
  moduleName = "Set"
  ; empty          = Set__empty
  ; insert         = Set__insert
  ; remove         = Set__remove
  ; mem            = Set__mem
  ; card           = Set__card
  ; eq             = Set__eq
  ; union          = Set__union
  ; diff           = Set__diff
  ; intersect      = Set__intersect
};
