// Testing utility

// expects 2 elements in results for each one in etalon
// asserts that those compare equal

func verify<T>(etalon : [T], results : [T], cmp : (T, T) -> Bool) {
    //print "\n";
    for (i in etalon.keys())
    { //printInt i; print "\n";
      assert cmp(etalon[i], results[i * 2]);
      assert cmp(etalon[i], results[i * 2 + 1]);
    }
};

func intCompare (a : Int, b : Int) : Bool = a == b;

// Numeric operators

func testNat(a : Nat, b : Nat) : [Nat] {
  let sum1 = a + b;
  let sum2 = (a + b) : Nat;
  let diff1 = a - b;
  let diff2 = (a - b) : Nat;
  let prod1 = a * b;
  let prod2 = (a * b) : Nat;
  let rat1 = a / b;
  let rat2 = (a / b) : Nat;
  let mod1 = a % b;
  let mod2 = (a % b) : Nat;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Nat;
  [sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

verify<Nat>([8, 2, 15, 1, 2, 125], testNat(5, 3), func (a : Nat, b : Nat) : Bool = a == b);

func testInt(a : Int, b : Int) : [Int] {
  let pos1 = + a;
  let pos2 = (+ a) : Int;
  let neg1 = - a;
  let neg2 = (- a) : Int;
  let sum1 = a + b;
  let sum2 = (a + b) : Int;
  let diff1 = a - b;
  let diff2 = (a - b) : Int;
  let prod1 = a * b;
  let prod2 = (a * b) : Int;
  let rat1 = a / b;
  let rat2 = (a / b) : Int;
  let mod1 = a % b;
  let mod2 = (a % b) : Int;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Int;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

verify<Int>([3, -3, 8, -2, 15, 0, 3, 243], testInt(3, 5), intCompare);

func testNatInt(a : Nat, b : Int) : [Int] {
  let pos1 = + a;
  let pos2 = (+ a) : Int;
  let neg1 = - a;
  let neg2 = (- a) : Int;
  let sum1 = a + b;
  let sum2 = (a + b) : Int;
  let sum3 = b + a;
  let sum4 = (b + a) : Int;
  let diff1 = a - b;
  let diff2 = (a - b) : Int;
  let diff3 = b - a;
  let diff4 = (b - a) : Int;
  let prod1 = a * b;
  let prod2 = (a * b) : Int;
  let prod3 = b * a;
  let prod4 = (b * a) : Int;
  let rat1 = a / b;
  let rat2 = (a / b) : Int;
  let rat3 = b / a;
  let rat4 = (b / a) : Int;
  let mod1 = a % b;
  let mod2 = (a % b) : Int;
  let mod3 = b % a;
  let mod4 = (b % a) : Int;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Int;
  let pow3 = b ** a;
  let pow4 = (b ** a) : Int;
  [pos1, pos2, neg1, neg2, sum1, sum2, sum3, sum4, diff1, diff2, diff3, diff4,
   prod1, prod2, prod3, prod4, rat1, rat2, rat3, rat4, mod1, mod2, mod3, mod4, pow1, pow2, pow3, pow4]
};

verify<Int>([3, -3, 8, 8, -2, 2, 15, 15, 0, 1, 3, 2, 243, 125], testNatInt(3, 5),
            intCompare);

func testFloat(a : Float, b : Float) : [Float] {
  let pos1 = + a;
  let pos2 = (+ a) : Float;
  let neg1 = - a;
  let neg2 = (- a) : Float;
  let sum1 = a + b;
  let sum2 = (a + b) : Float;
  let diff1 = a - b;
  let diff2 = (a - b) : Float;
  let prod1 = a * b;
  let prod2 = (a * b) : Float;
  let rat1 = a / b;
  let rat2 = (a / b) : Float;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Float;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, pow1, pow2]
};

// no Floats yet
// verify<Float>([3, -3, 8, -2, 15, 0, 3, 243], testFloat(3.0, 5.0), func (a : Float, b : Float) : Bool = a == b);

func testWord8(a : Word8, b : Word8) : [Word8] {
  let pos1 = + a;
  let pos2 = (+ a) : Word8;
  let neg1 = - a;
  let neg2 = (- a) : Word8;
  let sum1 = a + b;
  let sum2 = (a + b) : Word8;
  let diff1 = a - b;
  let diff2 = (a - b) : Word8;
  let prod1 = a * b;
  let prod2 = (a * b) : Word8;
  let rat1 = a / b;
  let rat2 = (a / b) : Word8;
  let mod1 = a % b;
  let mod2 = (a % b) : Word8;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Word8;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

func word8Compare(a : Word8, b : Word8) : Bool = a == b;
verify<Word8>([3, 253, 8, 254, 15, 0, 3, 243], testWord8(3, 5), word8Compare);

verify<Word8>([25, 231, 30, 20, 125, 5, 0, 249], testWord8(25, 5), word8Compare);

func testWord16(a : Word16, b : Word16) : [Word16] {
  let pos1 = + a;
  let pos2 = (+ a) : Word16;
  let neg1 = - a;
  let neg2 = (- a) : Word16;
  let sum1 = a + b;
  let sum2 = (a + b) : Word16;
  let diff1 = a - b;
  let diff2 = (a - b) : Word16;
  let prod1 = a * b;
  let prod2 = (a * b) : Word16;
  let rat1 = a / b;
  let rat2 = (a / b) : Word16;
  let mod1 = a % b;
  let mod2 = (a % b) : Word16;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Word16;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

func word16Compare(a : Word16, b : Word16) : Bool = a == b;
verify<Word16>([3, 65533, 8, 65534, 15, 0, 3, 243], testWord16(3, 5), word16Compare);
verify<Word16>([25, 65511, 30, 20, 125, 5, 0, 761], testWord16(25, 5), word16Compare);

func testWord32(a : Word32, b : Word32) : [Word32] {
  let pos1 = + a;
  let pos2 = (+ a) : Word32;
  let neg1 = - a;
  let neg2 = (- a) : Word32;
  let sum1 = a + b;
  let sum2 = (a + b) : Word32;
  let diff1 = a - b;
  let diff2 = (a - b) : Word32;
  let prod1 = a * b;
  let prod2 = (a * b) : Word32;
  let rat1 = a / b;
  let rat2 = (a / b) : Word32;
  let mod1 = a % b;
  let mod2 = (a % b) : Word32;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Word32;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

func word32Compare(a : Word32, b : Word32) : Bool = a == b;
verify<Word32>([3, 4_294_967_293, 8, 4_294_967_294, 15, 0, 3, 243], testWord32(3, 5), word32Compare);
verify<Word32>([25, 4_294_967_271, 30, 20, 125, 5, 0, 9765625], testWord32(25, 5), word32Compare);

func testWord64(a : Word64, b : Word64) : [Word64] {
  let pos1 = + a;
  let pos2 = (+ a) : Word64;
  let neg1 = - a;
  let neg2 = (- a) : Word64;
  let sum1 = a + b;
  let sum2 = (a + b) : Word64;
  let diff1 = a - b;
  let diff2 = (a - b) : Word64;
  let prod1 = a * b;
  let prod2 = (a * b) : Word64;
  let rat1 = a / b;
  let rat2 = (a / b) : Word64;
  let mod1 = a % b;
  let mod2 = (a % b) : Word64;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Word64;
  [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

verify<Word64>([3, 18_446_744_073_709_551_613, 8, 18_446_744_073_709_551_614, 15, 0, 3, 243], testWord64(3, 5),
               func (a : Word64, b : Word64) : Bool = a == b);


func testInt64(a : Int64, b : Int64) : [Int64] {
  let pos1 = + a;
  let pos2 = (+ a) : Int64;
  let neg1 = - a;
  let neg2 = (- a) : Int64;
  let sum1 = a + b;
  let sum2 = (a + b) : Int64;
  let diff1 = a - b;
  let diff2 = (a - b) : Int64;
  let prod1 = a * b;
  let prod2 = (a * b) : Int64;
  let rat1 = a / b;
  let rat2 = (a / b) : Int64;
  let mod1 = a % b;
  let mod2 = (a % b) : Int64;
  if (b >= (0 : Int64))
  {
    let pow1 = a ** b;
    let pow2 = (a ** b) : Int64;
    [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
  }
  else
  {
    [pos1, pos2, neg1, neg2, sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2]
  }
};

verify<Int64>([3, -3, 8, -2, 15, 0, 3, 243], testInt64(3, 5),
               func (a : Int64, b : Int64) : Bool = a == b);
verify<Int64>([13, -13, 18, 8, 65, 2, 3, 371293], testInt64(13, 5),
               func (a : Int64, b : Int64) : Bool = a == b);
verify<Int64>([-13, 13, -18, -8, 65, 2, -3], testInt64(-13, -5),
               func (a : Int64, b : Int64) : Bool = a == b);


func testInt32(a : Int32, b : Int32) : [Int32] {
  let pos1 = + a;
  let pos2 = (+ a) : Int32;
  let neg1 = - a;
  let neg2 = (- a) : Int32;
  let sum1 = a + b;
  let sum2 = (a + b) : Int32;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Int32;
  let prod1 = a * b;
  let prod2 = (a * b) : Int32;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Int32;
  let mod1 = a % b;
  let mod2 = (a % b) : Int32;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Int32;*/
  [pos1, pos2, neg1, neg2, sum1, sum2, /*diff1, diff2, prod1, prod2, */rat1, rat2, mod1, mod2/*, pow1, pow2*/]
};

verify<Int32>([3, -3, 8/*, -2, 15*/, 0, 3/*, 243*/], testInt32(3, 5),
             func (a : Int32, b : Int32) : Bool = a == b);
verify<Int32>([13, -13, 18/*, -2, 15*/, 2, 3/*, 243*/], testInt32(13, 5),
             func (a : Int32, b : Int32) : Bool = a == b);
verify<Int32>([-13, 13, -18/*, -2, 15*/, 2, -3/*, 243*/], testInt32(-13, -5),
             func (a : Int32, b : Int32) : Bool = a == b);


func testInt16(a : Int16, b : Int16) : [Int16] {
  let pos1 = + a;
  let pos2 = (+ a) : Int16;
  let neg1 = - a;
  let neg2 = (- a) : Int16;
  let sum1 = a + b;
  let sum2 = (a + b) : Int16;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Int16;
  let prod1 = a * b;
  let prod2 = (a * b) : Int16;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Int16;
  let mod1 = a % b;
  let mod2 = (a % b) : Int16;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Int16;*/
  [pos1, pos2, neg1, neg2, sum1, sum2, /*diff1, diff2, prod1, prod2, */rat1, rat2, mod1, mod2/*, pow1, pow2*/]
};

verify<Int16>([3, -3, 8/*, -2, 15, 0, 3, 243*/], testInt16(3, 5),
             func (a : Int16, b : Int16) : Bool = a == b);
verify<Int16>([13, -13, 18/*, -2, 15*/, 2, 3/*, 243*/], testInt16(13, 5),
             func (a : Int16, b : Int16) : Bool = a == b);
verify<Int16>([-13, 13, -18/*, -2, 15*/, 2, -3/*, 243*/], testInt16(-13, -5),
             func (a : Int16, b : Int16) : Bool = a == b);


func testInt8(a : Int8, b : Int8) : [Int8] {
  let pos1 = + a;
  let pos2 = (+ a) : Int8;
  let neg1 = - a;
  let neg2 = (- a) : Int8;
  let sum1 = a + b;
  let sum2 = (a + b) : Int8;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Int8;
  let prod1 = a * b;
  let prod2 = (a * b) : Int8;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Int8;
  let mod1 = a % b;
  let mod2 = (a % b) : Int8;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Int8;*/
  [pos1, pos2, neg1, neg2, sum1, sum2/*, diff1, diff2, prod1, prod2*/, rat1, rat2, mod1, mod2, /*pow1, pow2*/]
};

verify<Int8>([3, -3, 8/*, -2, 15*/, 0, 3/*, 243*/], testInt8(3, 5),
             func (a : Int8, b : Int8) : Bool = a == b);
verify<Int8>([13, -13, 18/*, -2, 15*/, 2, 3/*, 243*/], testInt8(13, 5),
             func (a : Int8, b : Int8) : Bool = a == b);
//verify<Int8>([3, -3, -2/*, -2, 15*/, 0, 3/*, 243*/], testInt8(3, -5),
//             func (a : Int8, b : Int8) : Bool = a == b);
//verify<Int8>([13, -13, 8/*, -2, 15*/, 0, 3/*, 243*/], testInt8(13, -5),
//             func (a : Int8, b : Int8) : Bool = a == b);
verify<Int8>([-13, 13, -18/*, -2, 15*/, 2, -3/*, 243*/], testInt8(-13, -5),
             func (a : Int8, b : Int8) : Bool = a == b);


func testNat64(a : Nat64, b : Nat64) : [Nat64] {
  let sum1 = a + b;
  let sum2 = (a + b) : Nat64;
  let diff1 = a - b;
  let diff2 = (a - b) : Nat64;
  let prod1 = a * b;
  let prod2 = (a * b) : Nat64;
  let rat1 = a / b;
  let rat2 = (a / b) : Nat64;
  let mod1 = a % b;
  let mod2 = (a % b) : Nat64;
  let pow1 = a ** b;
  let pow2 = (a ** b) : Nat64;
  [sum1, sum2, diff1, diff2, prod1, prod2, rat1, rat2, mod1, mod2, pow1, pow2]
};

verify<Nat64>([8, 2, 15, 1, 2, 125], testNat64(5, 3),
               func (a : Nat64, b : Nat64) : Bool = a == b);
verify<Nat64>([18, 8, 65, 2, 3, 371293], testNat64(13, 5),
               func (a : Nat64, b : Nat64) : Bool = a == b);


func testNat32(a : Nat32, b : Nat32) : [Nat32] {
  let sum1 = a + b;
  let sum2 = (a + b) : Nat32;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Nat32;
  let prod1 = a * b;
  let prod2 = (a * b) : Nat32;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Nat32;
  let mod1 = a % b;
  let mod2 = (a % b) : Nat32;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Nat32;*/
  [sum1, sum2/*, diff1, diff2, prod1, prod2*/, rat1, rat2, mod1, mod2/*, pow1, pow2*/]
};

verify<Nat32>([8/*, -2, 15*/, 1, 2/*, 243*/], testNat32(5, 3),
             func (a : Nat32, b : Nat32) : Bool = a == b);
verify<Nat32>([18/*, -2, 15*/, 5, 0/*, 243*/], testNat32(15, 3),
             func (a : Nat32, b : Nat32) : Bool = a == b);


func testNat16(a : Nat16, b : Nat16) : [Nat16] {
  let sum1 = a + b;
  let sum2 = (a + b) : Nat16;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Nat16;
  let prod1 = a * b;
  let prod2 = (a * b) : Nat16;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Nat16;
  let mod1 = a % b;
  let mod2 = (a % b) : Nat16;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Nat16;*/
  [sum1, sum2/*, diff1, diff2, prod1, prod2*/, rat1, rat2, mod1, mod2/*, pow1, pow2*/]
};

verify<Nat16>([8/*, -2, 15*/, 1, 2/*, 243*/], testNat16(5, 3),
             func (a : Nat16, b : Nat16) : Bool = a == b);
verify<Nat16>([18/*, -2, 15*/, 5, 0/*, 243*/], testNat16(15, 3),
             func (a : Nat16, b : Nat16) : Bool = a == b);


func testNat8(a : Nat8, b : Nat8) : [Nat8] {
  let sum1 = a + b;
  let sum2 = (a + b) : Nat8;
  /*let diff1 = a - b;
  let diff2 = (a - b) : Nat8;
  let prod1 = a * b;
  let prod2 = (a * b) : Nat8;*/
  let rat1 = a / b;
  let rat2 = (a / b) : Nat8;
  let mod1 = a % b;
  let mod2 = (a % b) : Nat8;
  /*let pow1 = a ** b;
  let pow2 = (a ** b) : Nat8;*/
  [sum1, sum2/*, diff1, diff2, prod1, prod2*/, rat1, rat2, mod1, mod2/*, pow1, pow2*/]
};

verify<Nat8>([8/*, -2, 15*/, 1, 2/*, 243*/], testNat8(5, 3),
             func (a : Nat8, b : Nat8) : Bool = a == b);
verify<Nat8>([18/*, -2, 15*/, 5, 0/*, 243*/], testNat8(15, 3),
             func (a : Nat8, b : Nat8) : Bool = a == b);
