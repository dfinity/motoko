// import Set

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