/** 

 Hash module: Hashes and hashing
 ===============================
 
 */

//type Hash32 = Word32
//type Hash = Hash32;


/////////////////////////////////////////////////////////////////////////////////////

// TEMP: A "bit string" as a linked list of bits:
type Bits = ?(Bool, Bits);

// TODO: Replace this definition WordX, for some X, once we have these types in AS.
type Hash = Bits;
//type Hash = Word16;
//type Hash = Word8;

/**
 Temp helpers for hashing
 ---------------------------
 Until we use a real hash function, we need various helper functions here.  They are uninteresting.
   
 */

// TODO: Replace with bitwise operations on Words, once we have each of those in AS.
// For now, we encode hashes as lists of booleans.
func getHashBit(h:Hash, pos:Nat) : Bool {
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

// Test if two lists of bits are equal.
func hashEq(ha:Hash, hb:Hash) : Bool {
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
