(* numeric operators *)
func TestInt(a:Int,b:Int):() {
   let pos1 = + a;
   let pos2 = (+ a) :Int;
   let neg1 = - a;
   let neg2 = (- a) :Int;
   let sum1 = a+b;
   let sum2 = (a+b):Int;
   let diff1 = a-b;
   let diff2 = (a-b):Int;
   let prod1 = a*b;
   let prod2 = (a*b):Int;
   let rat1 = a/b;
   let rat2 = (a/b):Int;
   let mod1 = a % b;
   let mod2 = (a % b):Int;
};


func TestFloat(a:Float,b:Float):() {
   let pos1 = + a;
   let pos2 = (+ a) :Float;
   let neg1 = - a;
   let neg2 = (- a) :Float;
   let sum1 = a+b;
   let sum2 = (a+b):Float;
   let diff1 = a-b;
   let diff2 = (a-b):Float;
   let prod1 = a*b;
   let prod2 = (a*b):Float;
   let rat1 = a/b;
   let rat2 = (a/b):Float;
   let mod1 = a % b;
   let mod2 = (a % b):Float;
};

(* TBR: do we want +/- _ and  _ - _ on Nat?*)
func TestNat(a:Nat,b:Nat):() {
   let pos1 = + a;
   let pos2 = (+ a) :Nat;
   let neg1 = - a;
   let neg2 = (- a) :Nat;
   let sum1 = a+b;
   let sum2 = (a+b):Nat;
   let diff1 = a-b;
   let diff2 = (a-b):Nat;
   let prod1 = a*b;
   let prod2 = (a*b):Nat;
   let rat1 = a/b;
   let rat2 = (a/b):Nat;
   let mod1 = a % b;
   let mod2 = (a % b):Nat;
};


func TestWord8(a:Word8,b:Word8):() {
   let pos1 = + a;
   let pos2 = (+ a) :Word8;
   let neg1 = - a;
   let neg2 = (- a) :Word8;
   let sum1 = a+b;
   let sum2 = (a+b):Word8;
   let diff1 = a-b;
   let diff2 = (a-b):Word8;
   let prod1 = a*b;
   let prod2 = (a*b):Word8;
   let rat1 = a/b;
   let rat2 = (a/b):Word8;
   let mod1 = a % b;
   let mod2 = (a % b):Word8;
};

func TestWord16(a:Word16,b:Word16):() {
   let pos1 = + a;
   let pos2 = (+ a) :Word16;
   let neg1 = - a;
   let neg2 = (- a) :Word16;
   let sum1 = a+b;
   let sum2 = (a+b):Word16;
   let diff1 = a-b;
   let diff2 = (a-b):Word16;
   let prod1 = a*b;
   let prod2 = (a*b):Word16;
   let rat1 = a/b;
   let rat2 = (a/b):Word16;
   let mod1 = a % b;
   let mod2 = (a % b):Word16;
};

func TestWord32(a:Word32,b:Word32):() {
   let pos1 = + a;
   let pos2 = (+ a) :Word32;
   let neg1 = - a;
   let neg2 = (- a) :Word32;
   let sum1 = a+b;
   let sum2 = (a+b):Word32;
   let diff1 = a-b;
   let diff2 = (a-b):Word32;
   let prod1 = a*b;
   let prod2 = (a*b):Word32;
   let rat1 = a/b;
   let rat2 = (a/b):Word32;
   let mod1 = a % b;
   let mod2 = (a % b):Word32;
};

func TestWord64(a:Word64,b:Word64):() {
   let pos1 = + a;
   let pos2 = (+ a) :Word64;
   let neg1 = - a;
   let neg2 = (- a) :Word64;
   let sum1 = a+b;
   let sum2 = (a+b):Word64;
   let diff1 = a-b;
   let diff2 = (a-b):Word64;
   let prod1 = a*b;
   let prod2 = (a*b):Word64;
   let rat1 = a/b;
   let rat2 = (a/b):Word64;
   let mod1 = a % b;
   let mod2 = (a % b):Word64;
};