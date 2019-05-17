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

verify<Word8>([3, 253, 8, 254, 15, 0, 3, 243], testWord8(3, 5), func (a : Word8, b : Word8) : Bool = a == b);

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

verify<Word16>([3, 65533, 8, 65534, 15, 0, 3, 243], testWord16(3, 5), func (a : Word16, b : Word16) : Bool = a == b);

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

verify<Word32>([3, 4_294_967_293, 8, 4_294_967_294, 15, 0, 3, 243], testWord32(3, 5),
               func (a : Word32, b : Word32) : Bool = a == b);

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
