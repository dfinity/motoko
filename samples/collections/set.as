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
  // Meta-level stuff:
  // ---------------------
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


////////////////////////////////////////////////////////////////////

func SetDb__print(s:Set<Nat>) {
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

func natEq(n:Nat,m:Nat):Bool{ n == m};

func SetDb__insert(s:Set<Nat>, x:Nat, xh:Hash):Set<Nat> = {
  print "  setInsert(";
  printInt x;
  print ")";
  let r = Set.insert<Nat>(s,x,xh);
  print ";\n";
  SetDb__print(r);
  r
};

func SetDb__mem(s:Set<Nat>, sname:Text, x:Nat, xh:Hash):Bool = {
  print "  setMem(";
  print sname;
  print ", ";
  printInt x;
  print ")";
  let b = Set.mem<Nat>(s,x,xh,natEq);
  if b { print " = true" } else { print " = false" };
  print ";\n";
  b
};

func SetDb__union(s1:Set<Nat>, s1name:Text, s2:Set<Nat>, s2name:Text):Set<Nat> = {
  print "  setUnion(";
  print s1name;
  print ", ";
  print s2name;
  print ")";
  // also: test that merge agrees with disj:
  let r1 = Set.union<Nat>(s1, s2);
  let r2 = Trie.disj<Nat,(),(),()>(s1, s2, natEq, func (_:?(),_:?()):(())=());
  assert(Trie.equalStructure<Nat,()>(r1, r2, natEq, Set__unitEq));
  print ";\n";
  SetDb__print(r1);
  print "=========\n";
  SetDb__print(r2);
  r1
};

func SetDb__intersect(s1:Set<Nat>, s1name:Text, s2:Set<Nat>, s2name:Text):Set<Nat> = {
  print "  setIntersect(";
  print s1name;
  print ", ";
  print s2name;
  print ")";
  let r = Set.intersect<Nat>(s1, s2, natEq);
  print ";\n";
  SetDb__print(r);
  r
};

/////////////////////////////////////////////////////////////////////////////////

// Create a record,
// as a standin until we have "real" modules to create namespaces:
let SetDb = new {
  // Meta-level stuff:
  // ---------------------
  moduleName = "SetDb"
  ; insert         = SetDb__insert
  ; mem            = SetDb__mem
  ; union          = SetDb__union
  ; intersect      = SetDb__intersect
};

/////////////////////////////////////////////////////////////////////////////////

func SetDb__test() {
  let hash_0 = ?(false,?(false,?(false,?(false, null))));
  let hash_1 = ?(false,?(false,?(false,?(true,  null))));
  let hash_2 = ?(false,?(false,?(true, ?(false, null))));
  let hash_3 = ?(false,?(false,?(true, ?(true,  null))));
  let hash_4 = ?(false,?(true, ?(false,?(false, null))));
  let hash_5 = ?(false,?(true, ?(false,?(true,  null))));
  let hash_6 = ?(false,?(true, ?(true, ?(false, null))));
  let hash_7 = ?(false,?(true, ?(true, ?(true,  null))));
  let hash_8 = ?(true, ?(false,?(false,?(false, null))));

  print "inserting...\n";
  // Insert numbers [0..8] into the set, using their bits as their hashes:
  let s0 : Set<Nat> = Set.empty<Nat>();
  assert(Set.card<Nat>(s0) == 0);

  let s1 : Set<Nat> = SetDb.insert(s0, 0, hash_0);
  assert(Set.card<Nat>(s1) == 1);

  let s2 : Set<Nat> = SetDb.insert(s1, 1, hash_1);
  assert(Set.card<Nat>(s2) == 2);

  let s3 : Set<Nat> = SetDb.insert(s2, 2, hash_2);
  assert(Set.card<Nat>(s3) == 3);

  let s4 : Set<Nat> = SetDb.insert(s3, 3, hash_3);
  assert(Set.card<Nat>(s4) == 4);

  let s5 : Set<Nat> = SetDb.insert(s4, 4, hash_4);
  assert(Set.card<Nat>(s5) == 5);

  let s6 : Set<Nat> = SetDb.insert(s5, 5, hash_5);
  assert(Set.card<Nat>(s6) == 6);

  let s7 : Set<Nat> = SetDb.insert(s6, 6, hash_6);
  assert(Set.card<Nat>(s7) == 7);

  let s8 : Set<Nat> = SetDb.insert(s7, 7, hash_7);
  assert(Set.card<Nat>(s8) == 8);

  let s9 : Set<Nat> = SetDb.insert(s8, 8, hash_8);
  assert(Set.card<Nat>(s9) == 9);
  print "done.\n";

  print "unioning...\n";
  let s1s2 : Set<Nat> = SetDb.union(s1, "s1", s2, "s2");
  let s2s1 : Set<Nat> = SetDb.union(s2, "s2", s1, "s1");
  let s3s2 : Set<Nat> = SetDb.union(s3, "s3", s2, "s2");
  let s4s2 : Set<Nat> = SetDb.union(s4, "s4", s2, "s2");
  let s1s5 : Set<Nat> = SetDb.union(s1, "s1", s5, "s5");
  let s0s2 : Set<Nat> = SetDb.union(s0, "s0", s2, "s2");
  print "done.\n";

  print "intersecting...\n";
  let s3is6 : Set<Nat> = SetDb.intersect(s3, "s3", s6, "s6");
  let s2is1 : Set<Nat> = SetDb.intersect(s2, "s2", s1, "s1");
  print "done.\n";


  print "testing membership...\n";

  // Element 0: Test memberships of each set defined above for element 0
  assert( not( SetDb.mem(s0, "s0", 0, hash_0 ) ));
  assert( SetDb.mem(s1, "s1", 0, hash_0 ) );
  assert( SetDb.mem(s2, "s2", 0, hash_0 ) );
  assert( SetDb.mem(s3, "s3", 0, hash_0 ) );
  assert( SetDb.mem(s4, "s4", 0, hash_0 ) );
  assert( SetDb.mem(s5, "s5", 0, hash_0 ) );
  assert( SetDb.mem(s6, "s6", 0, hash_0 ) );
  assert( SetDb.mem(s7, "s7", 0, hash_0 ) );
  assert( SetDb.mem(s8, "s8", 0, hash_0 ) );
  assert( SetDb.mem(s9, "s9", 0, hash_0 ) );

  // Element 1: Test memberships of each set defined above for element 1
  assert( not(SetDb.mem(s0, "s0", 1, hash_1 )) );
  assert( not(SetDb.mem(s1, "s1", 1, hash_1 )) );
  assert( SetDb.mem(s2, "s2", 1, hash_1 ) );
  assert( SetDb.mem(s3, "s3", 1, hash_1 ) );
  assert( SetDb.mem(s4, "s4", 1, hash_1 ) );
  assert( SetDb.mem(s5, "s5", 1, hash_1 ) );
  assert( SetDb.mem(s6, "s6", 1, hash_1 ) );
  assert( SetDb.mem(s7, "s7", 1, hash_1 ) );
  assert( SetDb.mem(s8, "s8", 1, hash_1 ) );
  assert( SetDb.mem(s9, "s9", 1, hash_1 ) );

  // Element 2: Test memberships of each set defined above for element 2
  assert( not(SetDb.mem(s0, "s0", 2, hash_2 )) );
  assert( not(SetDb.mem(s1, "s1", 2, hash_2 )) );
  assert( not(SetDb.mem(s2, "s2", 2, hash_2 )) );
  assert( SetDb.mem(s3, "s3", 2, hash_2 ) );
  assert( SetDb.mem(s4, "s4", 2, hash_2 ) );
  assert( SetDb.mem(s5, "s5", 2, hash_2 ) );
  assert( SetDb.mem(s6, "s6", 2, hash_2 ) );
  assert( SetDb.mem(s7, "s7", 2, hash_2 ) );
  assert( SetDb.mem(s8, "s8", 2, hash_2 ) );
  assert( SetDb.mem(s9, "s9", 2, hash_2 ) );

  // Element 3: Test memberships of each set defined above for element 3
  assert( not(SetDb.mem(s0, "s0", 3, hash_3 )) );
  assert( not(SetDb.mem(s1, "s1", 3, hash_3 )) );
  assert( not(SetDb.mem(s2, "s2", 3, hash_3 )) );
  assert( not(SetDb.mem(s3, "s3", 3, hash_3 )) );
  assert( SetDb.mem(s4, "s4", 3, hash_3 ) );
  assert( SetDb.mem(s5, "s5", 3, hash_3 ) );
  assert( SetDb.mem(s6, "s6", 3, hash_3 ) );
  assert( SetDb.mem(s7, "s7", 3, hash_3 ) );
  assert( SetDb.mem(s8, "s8", 3, hash_3 ) );
  assert( SetDb.mem(s9, "s9", 3, hash_3 ) );

  // Element 4: Test memberships of each set defined above for element 4
  assert( not(SetDb.mem(s0, "s0", 4, hash_4 )) );
  assert( not(SetDb.mem(s1, "s1", 4, hash_4 )) );
  assert( not(SetDb.mem(s2, "s2", 4, hash_4 )) );
  assert( not(SetDb.mem(s3, "s3", 4, hash_4 )) );
  assert( not(SetDb.mem(s4, "s4", 4, hash_4 )) );
  assert( SetDb.mem(s5, "s5", 4, hash_4 ) );
  assert( SetDb.mem(s6, "s6", 4, hash_4 ) );
  assert( SetDb.mem(s7, "s7", 4, hash_4 ) );
  assert( SetDb.mem(s8, "s8", 4, hash_4 ) );
  assert( SetDb.mem(s9, "s9", 4, hash_4 ) );

  // Element 5: Test memberships of each set defined above for element 5
  assert( not(SetDb.mem(s0, "s0", 5, hash_5 )) );
  assert( not(SetDb.mem(s1, "s1", 5, hash_5 )) );
  assert( not(SetDb.mem(s2, "s2", 5, hash_5 )) );
  assert( not(SetDb.mem(s3, "s3", 5, hash_5 )) );
  assert( not(SetDb.mem(s4, "s4", 5, hash_5 )) );
  assert( not(SetDb.mem(s5, "s5", 5, hash_5 )) );
  assert( SetDb.mem(s6, "s6", 5, hash_5 ) );
  assert( SetDb.mem(s7, "s7", 5, hash_5 ) );
  assert( SetDb.mem(s8, "s8", 5, hash_5 ) );
  assert( SetDb.mem(s9, "s9", 5, hash_5 ) );

  // Element 6: Test memberships of each set defined above for element 6
  assert( not(SetDb.mem(s0, "s0", 6, hash_6 )) );
  assert( not(SetDb.mem(s1, "s1", 6, hash_6 )) );
  assert( not(SetDb.mem(s2, "s2", 6, hash_6 )) );
  assert( not(SetDb.mem(s3, "s3", 6, hash_6 )) );
  assert( not(SetDb.mem(s4, "s4", 6, hash_6 )) );
  assert( not(SetDb.mem(s5, "s5", 6, hash_6 )) );
  assert( not(SetDb.mem(s6, "s6", 6, hash_6 )) );
  assert( SetDb.mem(s7, "s7", 6, hash_6 ) );
  assert( SetDb.mem(s8, "s8", 6, hash_6 ) );
  assert( SetDb.mem(s9, "s9", 6, hash_6 ) );

  // Element 7: Test memberships of each set defined above for element 7
  assert( not(SetDb.mem(s0, "s0", 7, hash_7 )) );
  assert( not(SetDb.mem(s1, "s1", 7, hash_7 )) );
  assert( not(SetDb.mem(s2, "s2", 7, hash_7 )) );
  assert( not(SetDb.mem(s3, "s3", 7, hash_7 )) );
  assert( not(SetDb.mem(s4, "s4", 7, hash_7 )) );
  assert( not(SetDb.mem(s5, "s5", 7, hash_7 )) );
  assert( not(SetDb.mem(s6, "s6", 7, hash_7 )) );
  assert( not(SetDb.mem(s7, "s7", 7, hash_7 )) );
  assert( SetDb.mem(s8, "s8", 7, hash_7 ) );
  assert( SetDb.mem(s9, "s9", 7, hash_7 ) );

  // Element 8: Test memberships of each set defined above for element 8
  assert( not(SetDb.mem(s0, "s0", 8, hash_8 )) );
  assert( not(SetDb.mem(s1, "s1", 8, hash_8 )) );
  assert( not(SetDb.mem(s2, "s2", 8, hash_8 )) );
  assert( not(SetDb.mem(s3, "s3", 8, hash_8 )) );
  assert( not(SetDb.mem(s4, "s4", 8, hash_8 )) );
  assert( not(SetDb.mem(s6, "s6", 8, hash_8 )) );
  assert( not(SetDb.mem(s6, "s6", 8, hash_8 )) );
  assert( not(SetDb.mem(s7, "s7", 8, hash_8 )) );
  assert( not(SetDb.mem(s8, "s8", 8, hash_8 )) );
  assert( SetDb.mem(s9, "s9", 8, hash_8 ) );

  print "done.\n";
};
