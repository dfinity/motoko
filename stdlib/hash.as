/**

Hash module: Hashes and hashing
===============================

Representations for `Hash` type
---------------------------------

We consider two representations for a hash value:

 - as a linked list of booleans (`BitList` below), or
 - as a `Word32`, or some other `Word` type; we call these "bit vectors" below.

The linked list of booleans is closest to the mathematical definition
of a (finite, but unbounded) "bit string", but the `Word32` provides
more efficient practical operations.  We didn't have access to these
operations, initially, during the first implementation.

*/

//type Hash32 = Word32
//type Hash = Hash32;


/////////////////////////////////////////////////////////////////////////////////////

/** A "bit string" as a linked list of bits: */
type BitList = ?(Bool, BitList);

/** A "bit vector" is a bounded-length bit string packed into a single word: */
type BitVec = Word32;

// TODO: Replace this definition with `BitVec`.
type Hash = BitList;
//type Hash = Word16;
//type Hash = Word8;

/**

 `BitVec`
 ----------------------
 A "bit vector" is a bounded-length bit string packed into a single word.

 */

let BitVec = new {

  func length() : Nat = 31;

  /** Test if two lists of bits are equal. */
  func getHashBit(h:BitVec, pos:Nat) : Bool {
    assert (pos <= length());
    if ((h & (natToWord32(1) << natToWord32(pos))) != natToWord32(0)) { true }
    else { false }
  };

  /** Test if two lists of bits are equal. */
  func hashEq(ha:BitVec, hb:BitVec) : Bool {
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

};

/**

 `BitList`
 ----------

 Encode hashes as lists of booleans.

 TODO: Replace with bitwise operations on Words, for greater efficiency.
*/
let BitList = new {

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
