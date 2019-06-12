let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Shared = prim "Shared";
type Null = prim "Null";
type Bool = prim "Bool";
type Nat = prim "Nat";
type Int = prim "Int";
type Word8 = prim "Word8";
type Word16 = prim "Word16";
type Word32 = prim "Word32";
type Word64 = prim "Word64";
type Float = prim "Float";
type Char = prim "Char";
type Text = prim "Text";

type Iter<T_> = {next : () -> ?T_};

func abs(x : Int) : Nat { (prim "abs" : Int -> Nat) x };

func ignore(_ : Any) {};

class range(x : Nat, y : Nat) {
  private var i = x;
  next() : ?Nat { if (i > y) null else {let j = i; i += 1; ?j} };
};

class revrange(x : Nat, y : Nat) {
  private var i = x + 1;
  next() : ?Nat { if (i <= y) null else {i -= 1; ?i} };
};

// for testing
func idlHash(x : Text) : Word32 { (prim "idlHash" : Text -> Word32) x };

func charToText(c : Char) : Text = (prim "Char->Text" : Char -> Text) c;

func printInt(x : Int) { print (@text_of_Int x) };
func printChar(x : Char) { print (charToText x) };
func print(x : Text) { (prim "print" : Text -> ()) x };
func rts_version() : Text { (prim "rts_version" : () -> Text) () };

// Hashing
func hashInt(x : Int) : Word32 {
  var n = x;
  var hash : Word32 = 0;
  if (n < 0) {
    hash := ^hash;
    n := abs n;
  };
  let base = 2**32;
  while (n > 0) {
    hash ^= intToWord32(n % base);
    n /= base;
  };
  return hash;
};

// Conversions
func natToWord8(n : Nat) : Word8 = (prim "Nat->Word8" : Nat -> Word8) n;
func word8ToNat(n : Word8) : Nat = (prim "Word8->Nat" : Word8 -> Nat) n;
func intToWord8(n : Int) : Word8 = (prim "Int->Word8" : Int -> Word8) n;
func word8ToInt(n : Word8) : Int = (prim "Word8->Int" : Word8 -> Int) n;

func natToWord16(n : Nat) : Word16 = (prim "Nat->Word16" : Nat -> Word16) n;
func word16ToNat(n : Word16) : Nat = (prim "Word16->Nat" : Word16 -> Nat) n;
func intToWord16(n : Int) : Word16 = (prim "Int->Word16" : Int -> Word16) n;
func word16ToInt(n : Word16) : Int = (prim "Word16->Int" : Word16 -> Int) n;

func natToWord32(n : Nat) : Word32 = (prim "Nat->Word32" : Nat -> Word32) n;
func word32ToNat(n : Word32) : Nat = (prim "Word32->Nat" : Word32 -> Nat) n;
func intToWord32(n : Int) : Word32 = (prim "Int->Word32" : Int -> Word32) n;
func word32ToInt(n : Word32) : Int = (prim "Word32->Int" : Word32 -> Int) n;

func natToWord64(n : Nat) : Word64 = (prim "Nat->Word64" : Nat -> Word64) n;
func word64ToNat(n : Word64) : Nat = (prim "Word64->Nat" : Word64 -> Nat) n;
func intToWord64(n : Int) : Word64 = (prim "Int->Word64" : Int -> Word64) n;
func word64ToInt(n : Word64) : Int = (prim "Word64->Int" : Word64 -> Int) n;

func charToWord32(c : Char) : Word32 = (prim "Char->Word32" : Char -> Word32) c;
func word32ToChar(w : Word32) : Char = (prim "Word32->Char" : Word32 -> Char) w;
func decodeUTF8(s : Text) : (Word32, Char) = (prim "decodeUTF8" : Text -> (Word32, Char)) s;

// Exotic bitwise operations
func popcntWord8(w : Word8) : Word8 = (prim "popcnt8" : Word8 -> Word8) w;
func clzWord8(w : Word8) : Word8 = (prim "clz8" : Word8 -> Word8) w;
func ctzWord8(w : Word8) : Word8 = (prim "ctz8" : Word8 -> Word8) w;
func btstWord8(w : Word8, amount : Word8) : Bool = (prim "btst8" : (Word8, Word8) -> Word8) (w, amount) != (0 : Word8);

func popcntWord16(w : Word16) : Word16 = (prim "popcnt16" : Word16 -> Word16) w;
func clzWord16(w : Word16) : Word16 = (prim "clz16" : Word16 -> Word16) w;
func ctzWord16(w : Word16) : Word16 = (prim "ctz16" : Word16 -> Word16) w;
func btstWord16(w : Word16, amount : Word16) : Bool = (prim "btst16" : (Word16, Word16) -> Word16) (w, amount) != (0 : Word16);

func popcntWord32(w : Word32) : Word32 = (prim "popcnt" : Word32 -> Word32) w;
func clzWord32(w : Word32) : Word32 = (prim "clz" : Word32 -> Word32) w;
func ctzWord32(w : Word32) : Word32 = (prim "ctz" : Word32 -> Word32) w;
func btstWord32(w : Word32, amount : Word32) : Bool = (prim "btst" : (Word32, Word32) -> Word32) (w, amount) != (0 : Word32);

func popcntWord64(w : Word64) : Word64 = (prim "popcnt64" : Word64 -> Word64) w;
func clzWord64(w : Word64) : Word64 = (prim "clz64" : Word64 -> Word64) w;
func ctzWord64(w : Word64) : Word64 = (prim "ctz64" : Word64 -> Word64) w;
func btstWord64(w : Word64, amount : Word64) : Bool = (prim "btst64" : (Word64, Word64) -> Word64) (w, amount) != (0 : Word64);

// Internal helper functions for the show translation

// The @ in the name ensures that this cannot be shadowed by user code, so
// compiler passes can rely on them being in scope
// The text_of functions do not need to be exposed; the user can just use
// the show above.

func @text_of_Nat(x : Nat) : Text {
  var text = "";
  var n = x;
  let base = 10;
  let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

  assert(n >= 0);

  if (n == 0) {
    return "0";
  };

  while (n > 0) {
    let rem = n % base;
    text := digits[rem] # text;
    n := n / base;
  };
  return text;
};

func @text_of_Int(x : Int) : Text {
  if (x == 0) {
    return "0";
  };
  if (x < 0) {
    "-" # @text_of_Nat(abs x)
  } else {
    @text_of_Nat(abs x)
  }
};

func @text_of_Bool(b : Bool) : Text {
  if (b) "true" else "false"
};

func @text_of_Text(t : Text) : Text {
  // TODO: Escape properly
  "\"" # t # "\"";
};

func @text_of_option<T>(f : T -> Text, x : ?T) : Text {
  switch (x) {
    case (?y) {"?(" # f y # ")"};
    case null {"null"};
  }
};

func @text_of_variant<T>(l : Text, f : T -> Text, x : T) : Text {
  let fx = f x;
  if (fx == "()") "(#" # l # ")"
  else "(#" # l # " " # fx # ")"
};

func @text_of_array<T>(f : T -> Text, xs : [T]) : Text {
  var text = "";
  for (x in xs.vals()) {
    if (text == "") {
      text := text # "[";
    } else {
      text := text # ", ";
    };
    text := text # f x;
  };
  text := text # "]";
  return text;
};

func @text_of_array_mut<T>(f : T -> Text, xs : [var T]) : Text {
  var text = "";
  for (x in xs.vals()) {
    if (text == "") {
      text := text # "[var ";
    } else {
      text := text # ", ";
    };
    text := text # f x;
  };
  text := text # "]";
  return text;
};

// Array utilities

// This would be nicer as a objects, but lets do them as functions
// until the compiler has a concept of “static objects”
func Array_init<T>(len : Nat,  x : T) : [var T] {
  (prim "Array.init" : <T>(Nat, T) -> [var T])<T>(len, x)
};

func Array_tabulate<T>(len : Nat,  gen : Nat -> T) : [T] {
  (prim "Array.tabulate" : <T>(Nat, Nat -> T) -> [T])<T>(len, gen)
};

type Cont<T <: Shared> = T -> () ;
type Async<T <: Shared> = Cont<T> -> ();

func @new_async<T <: Shared>():(Async<T>, Cont<T>) {
  let empty = func k (t:T) = ();
  var result : ?T = null;
  var ks : T -> () = empty;
  func fulfill(t:T):() {
    switch(result) {
      case null {
        result := ?t;
        let ks_ = ks;
        ks := empty;
        ks_(t);
      };
      case (?t) (assert(false));
      };
    };
  func enqueue(k:Cont<T>):() {
    switch(result) {
      case null {
        let ks_ = ks;
        ks := (func (t:T) {ks_(t);k(t);});
      };
      case (?t) (k(t));
    };
  };
  (enqueue,fulfill)
};
|}
