module {
/**

Hash values
===============================

Hash values represent a string of _hash bits_, and support associated _bit string_ operations.

Representations for `Hash` type
---------------------------------

We consider two representations for a hash value:

 - as a linked list of booleans, as `BitList` below; or,
 - as a "bit vector" packed into a `Word` type (viz., `Word32`).


### Why?

Initially, during the first implementation of some of the standard
library (e.g., for hash tries), we didn't have access to `Word`-based
operations and hence we instead used bit lists.

Bit lists are closest to the mathematical definition of finite, but
unbounded bit strings, but the `Word32` provides more efficient
practical operations.

Now, the language supports `Word` operations, so we will use bit
vectors as the preferred representation for practical system tests
going forward.

*/

/** A "bit string" as a linked list of bits: */
type BitList = ?(Bool, BitList);

/** A "bit vector" is a bounded-length bit string packed into a single word: */
type BitVec = Word32;

/**

 `BitVec`
 ----------------------
 A "bit vector" is a bounded-length bit string packed into a single word.

 */

module BitVec {

  type t = BitVec;

  func length() : Nat =
    label profile_hash_length : Nat
    31;

  func hashOfInt(i:Int) : BitVec {
    hashInt(i)
  };

  func hashOfIntAcc(h:BitVec, i:Int) : BitVec {
    //hashIntAcc(h, i)
    // xxx use the value h
    hashInt(i)
  };

  func hashOfText(t:Text) : BitVec {
    var x = 0 : Word32;
    for (c in t.chars()) {
      x := x ^ charToWord32(c);
    };
    return x
  };

  /** Project a given bit from the bit vector. */
  func getHashBit(h:BitVec, pos:Nat) : Bool =
    label profile_getHashBit : Bool {
    assert (pos <= length());
    if ((h & (natToWord32(1) << natToWord32(pos))) != natToWord32(0))
    { label profile_getHashBit_true : Bool
      true
    }
    else
    { label profile_getHashBit_false : Bool
      false
    }
  };

  /** Test if two lists of bits are equal. */
  func hashEq(ha:BitVec, hb:BitVec) : Bool {
    label profile_hashEq : Bool
    ha == hb
  };

  func bitsPrintRev(bits:BitVec) {
    for (j in range(0, length() - 1)) {
      if (getHashBit(bits, j)) {
        print "1"
      } else {
        print "0"
      }
    }
  };

  func hashPrintRev(bits:BitVec) {
    for (j in range(length() - 1, 0)) {
      if (getHashBit(bits, j)) {
        print "1"
      } else {
        print "0"
      }
    }
  };

  func toList(v:BitVec) : BitList {
    func rec(pos:Nat) : BitList {
      if (pos >= length()) { null }
      else {
        let rest = rec(pos + 1);
        if (getHashBit(v, pos)) { ?(true, rest) }
        else { ?(false, rest) }
      }
    };
    rec(0)
  }

};

/**

 `BitList`
 ----------

 Encode hashes as lists of booleans.

 TODO: Replace with bitwise operations on Words, for greater efficiency.
*/
module BitList {

  type t = BitList;

  func hashOfInt(i:Int) : BitList {
    BitVec.toList(BitVec.hashOfInt(i))
  };

  /** Test if two lists of bits are equal. */
  func getHashBit(h:BitList, pos:Nat) : Bool {
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

  /** Test if two lists of bits are equal. */
  func hashEq(ha:BitList, hb:BitList) : Bool {
    switch (ha, hb) {
    case (null, null) true;
    case (null, _) false;
    case (_, null) false;
    case (?(bita, ha2), ?(bitb, hb2)) {
	         if (bita == bitb) { hashEq(ha2, hb2) }
	         else { false }
	       };
    }
  };

  func bitsPrintRev(bits:BitList) {
	  switch bits {
	  case null { print "" };
	  case (?(bit,bits_)) {
		       bitsPrintRev(bits_);
		       if bit { print "1R." }
		       else   { print "0L." }
	       }
	  }
  };

  func hashPrintRev(bits:BitList) {
	  switch bits {
	  case null { print "" };
	  case (?(bit,bits_)) {
		       hashPrintRev(bits_);
		       if bit { print "1" }
		       else   { print "0" }
	       }
	  }
  };

};


/**
 Canonical representations
 ---------------------------

 Choose a canonical representation of hash values for the rest of
 the standard library to use:
*/

type Hash = BitVec;
let Hash = BitVec;
}
