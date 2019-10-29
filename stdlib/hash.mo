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
public type BitList = ?(Bool, BitList);

/** A "bit vector" is a bounded-length bit string packed into a single word: */
public type BitVec = Word32;

/**

 `BitVec`
 ----------------------
 A "bit vector" is a bounded-length bit string packed into a single word.

 */

public module BitVec {

  public type t = BitVec;

  // Jenkin's one at a time:
  // https://en.wikipedia.org/wiki/Jenkins_hash_function#one_at_a_time
  //
  // The input type should actually be [Word8].
  // Note: Be sure to explode each Word8 of a Word32 into its own Word32, and to shift into lower 8 bits.
  public func hashWord8s(key:[BitVec]) : BitVec = label profile_hash_hashWord8s : BitVec {
    var hash = natToWord32(0);
    for (wordOfKey in key.vals()) { label profile_hash_hashWord8s_forLoop : ()
      hash := hash + wordOfKey;
      hash := hash + hash << 10;
      hash := hash ^ (hash >> 6);
    };
    hash := hash + hash << 3;
    hash := hash ^ (hash >> 11);
    hash := hash + hash << 15;
    return hash;
  };

  public func length() : Nat =
    label profile_hash_length : Nat
    31;

  public func hashOfInt(i:Int) : BitVec = label profile_hash_hashOfInt : BitVec {
    //hashInt(i)
    let j = intToWord32(i);
    hashWord8s(
      [j & (255 << 0),
       j & (255 << 8),
       j & (255 << 16),
       j & (255 << 24)
      ]);
  };

  public func hashOfIntAcc(h1:BitVec, i:Int) : BitVec = label profile_hash_hashOfIntAcc : BitVec {
    let j = intToWord32(i);
    hashWord8s(
      [h1,
       j & (255 << 0),
       j & (255 << 8),
       j & (255 << 16),
       j & (255 << 24)
      ]);
  };

  public func hashOfText(t:Text) : BitVec  = label profile_hash_hashOfText : BitVec {
    var x = 0 : Word32;
    for (c in t.chars()) {
      x := x ^ charToWord32(c);
    };
    return x
  };

  /** Project a given bit from the bit vector. */
  public func getHashBit(h:BitVec, pos:Nat) : Bool = label profile_getHashBit : Bool {
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
  public func hashEq(ha:BitVec, hb:BitVec) : Bool {
    label profile_hashEq : Bool
    ha == hb
  };

  public func bitsPrintRev(bits:BitVec) {
    for (j in range(0, length() - 1)) {
      if (getHashBit(bits, j)) {
        print "1"
      } else {
        print "0"
      }
    }
  };

  public func hashPrintRev(bits:BitVec) {
    for (j in range(length() - 1, 0)) {
      if (getHashBit(bits, j)) {
        print "1"
      } else {
        print "0"
      }
    }
  };

  public func toList(v:BitVec) : BitList {
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
public module BitList {

  public type t = BitList;

  public func hashOfInt(i:Int) : BitList {
    BitVec.toList(BitVec.hashOfInt(i))
  };

  /** Test if two lists of bits are equal. */
  public func getHashBit(h:BitList, pos:Nat) : Bool {
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
  public func hashEq(ha:BitList, hb:BitList) : Bool {
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

  public func bitsPrintRev(bits:BitList) {
	  switch bits {
	  case null { print "" };
	  case (?(bit,bits_)) {
		       bitsPrintRev(bits_);
		       if bit { print "1R." }
		       else   { print "0L." }
	       }
	  }
  };

  public func hashPrintRev(bits:BitList) {
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

public type Hash = BitVec;
public let Hash = BitVec;
}
