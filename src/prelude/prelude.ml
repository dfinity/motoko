let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Null = prim "Null";
type Bool = prim "Bool";
type Nat = prim "Nat";
type Nat8 = prim "Nat8";
type Nat16 = prim "Nat16";
type Nat32 = prim "Nat32";
type Nat64 = prim "Nat64";
type Int = prim "Int";
type Int8 = prim "Int8";
type Int16 = prim "Int16";
type Int32 = prim "Int32";
type Int64 = prim "Int64";
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
  var i = x;
  public func next() : ?Nat { if (i > y) null else {let j = i; i += 1; ?j} };
};

class revrange(x : Nat, y : Nat) {
  var i = x + 1;
  public func next() : ?Nat { if (i <= y) null else {i -= 1; ?i} };
};

// Implementations for overloaded dot operations
// Note that these return functions!
// (Some optimizations in the backend might be feasible.)

func @immut_array_get<A>(xs : [A]) : (Nat -> A) =
  (func (n : Nat) : A = xs[n]);
func @mut_array_get<A>(xs : [var A]) : (Nat -> A) =
  (func (n : Nat) : A = xs[n]);
func @immut_array_len<A>(xs : [A]) : (() -> Nat) =
  (func () : Nat = (prim "array_len" : ([A]) -> Nat) xs);
func @mut_array_len<A>(xs : [var A]) : (() -> Nat) =
  (func () : Nat = (prim "array_len" : ([var A]) -> Nat) xs);
func @mut_array_set<A>(xs : [var A]) : ((Nat, A) -> ()) =
  (func (n : Nat, x : A) = (xs[n] := x));
func @immut_array_keys<A>(xs : [A]) : (() -> Iter<Nat>) =
  (func () : Iter<Nat> = object {
    var i = 0;
    let l = xs.len();
    public func next() : ?Nat { if (i >= l) null else {let j = i; i += 1; ?j} };
  });
func @mut_array_keys<A>(xs : [var A]) : (() -> Iter<Nat>) =
  (func () : Iter<Nat> = object {
    var i = 0;
    let l = xs.len();
    public func next() : ?Nat { if (i >= l) null else {let j = i; i += 1; ?j} };
  });
func @immut_array_vals<A>(xs : [A]) : (() -> Iter<A>) =
  (func () : Iter<A> = object {
    var i = 0;
    let l = xs.len();
    public func next() : ?A { if (i >= l) null else {let j = i; i += 1; ?xs[j]} };
  });
func @mut_array_vals<A>(xs : [var A]) : (() -> Iter<A>) =
  (func () : Iter<A> = object {
    var i = 0;
    let l = xs.len();
    public func next() : ?A { if (i >= l) null else {let j = i; i += 1; ?xs[j]} };
  });
func @text_len(xs : Text) : (() -> Nat) =
  (func () : Nat = (prim "text_len" : Text -> Nat) xs);
func @text_chars(xs : Text) : (() -> Iter<Char>) =
  (func () : Iter<Char> = (prim "text_chars" : Text -> Iter<Char>) xs);

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

func int64ToInt(n : Int64) : Int = (prim "Int64->Int" : Int64 -> Int) n;
func intToInt64(n : Int) : Int64 = (prim "Int->Int64" : Int -> Int64) n;
func int64ToWord64(n : Int64) : Word64 = (prim "Int64->Word64" : Int64 -> Word64) n;
func word64ToInt64(n : Word64) : Int64 = (prim "Word64->Int64" : Word64 -> Int64) n;
func int32ToInt(n : Int32) : Int = (prim "Int32->Int" : Int32 -> Int) n;
func intToInt32(n : Int) : Int32 = (prim "Int->Int32" : Int -> Int32) n;
func int32ToWord32(n : Int32) : Word32 = (prim "Int32->Word32" : Int32 -> Word32) n;
func word32ToInt32(n : Word32) : Int32 = (prim "Word32->Int32" : Word32 -> Int32) n;
func int16ToInt(n : Int16) : Int = (prim "Int16->Int" : Int16 -> Int) n;
func intToInt16(n : Int) : Int16 = (prim "Int->Int16" : Int -> Int16) n;
func int16ToWord16(n : Int16) : Word16 = (prim "Int16->Word16" : Int16 -> Word16) n;
func word16ToInt16(n : Word16) : Int16 = (prim "Word16->Int16" : Word16 -> Int16) n;
func int8ToInt(n : Int8) : Int = (prim "Int8->Int" : Int8 -> Int) n;
func intToInt8(n : Int) : Int8 = (prim "Int->Int8" : Int -> Int8) n;
func int8ToWord8(n : Int8) : Word8 = (prim "Int8->Word8" : Int8 -> Word8) n;
func word8ToInt8(n : Word8) : Int8 = (prim "Word8->Int8" : Word8 -> Int8) n;

func nat64ToNat(n : Nat64) : Nat = (prim "Nat64->Nat" : Nat64 -> Nat) n;
func natToNat64(n : Nat) : Nat64 = (prim "Nat->Nat64" : Nat -> Nat64) n;
func nat64ToWord64(n : Nat64) : Word64 = (prim "Nat64->Word64" : Nat64 -> Word64) n;
func word64ToNat64(n : Word64) : Nat64 = (prim "Word64->Nat64" : Word64 -> Nat64) n;
func nat32ToNat(n : Nat32) : Nat = (prim "Nat32->Nat" : Nat32 -> Nat) n;
func natToNat32(n : Nat) : Nat32 = (prim "Nat->Nat32" : Nat -> Nat32) n;
func nat32ToWord32(n : Nat32) : Word32 = (prim "Nat32->Word32" : Nat32 -> Word32) n;
func word32ToNat32(n : Word32) : Nat32 = (prim "Word32->Nat32" : Word32 -> Nat32) n;
func nat16ToNat(n : Nat16) : Nat = (prim "Nat16->Nat" : Nat16 -> Nat) n;
func natToNat16(n : Nat) : Nat16 = (prim "Nat->Nat16" : Nat -> Nat16) n;
func nat16ToWord16(n : Nat16) : Word16 = (prim "Nat16->Word16" : Nat16 -> Word16) n;
func word16ToNat16(n : Word16) : Nat16 = (prim "Word16->Nat16" : Word16 -> Nat16) n;
func nat8ToNat(n : Nat8) : Nat = (prim "Nat8->Nat" : Nat8 -> Nat) n;
func natToNat8(n : Nat) : Nat8 = (prim "Nat->Nat8" : Nat -> Nat8) n;
func nat8ToWord8(n : Nat8) : Word8 = (prim "Nat8->Word8" : Nat8 -> Word8) n;
func word8ToNat8(n : Word8) : Nat8 = (prim "Word8->Nat8" : Word8 -> Nat8) n;


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

func @text_of_Nat8(x : Nat8) : Text = @text_of_Nat (nat8ToNat x);
func @text_of_Nat16(x : Nat16) : Text = @text_of_Nat (nat16ToNat x);
func @text_of_Nat32(x : Nat32) : Text = @text_of_Nat (nat32ToNat x);
func @text_of_Nat64(x : Nat64) : Text = @text_of_Nat (nat64ToNat x);
func @text_of_Int8(x : Int8) : Text = @text_of_Int (int8ToInt x);
func @text_of_Int16(x : Int16) : Text = @text_of_Int (int16ToInt x);
func @text_of_Int32(x : Int32) : Text = @text_of_Int (int32ToInt x);
func @text_of_Int64(x : Int64) : Text = @text_of_Int (int64ToInt x);
func @text_of_Word8(x : Word8) : Text = @text_of_Nat (word8ToNat x);
func @text_of_Word16(x : Word16) : Text = @text_of_Nat (word16ToNat x);
func @text_of_Word32(x : Word32) : Text = @text_of_Nat (word32ToNat x);
func @text_of_Word64(x : Word64) : Text = @text_of_Nat (word64ToNat x);


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

type Cont<T> = T -> () ;
type Async<T> = Cont<T> -> ();

func @new_async<T <: Any>() : (Async<T>, Cont<T>) {
  let empty = func(t : T) {};
  var result : ?T = null;
  var ks : T -> () = empty;

  func fulfill(t : T) {
    switch result {
      case null {
        result := ?t;
        let ks_ = ks;
        ks := empty;
        ks_(t);
      };
      case (?t) { assert false };
    };
  };

  func enqueue(k : Cont<T>) {
    switch result {
      case null {
        let ks_ = ks;
        ks := (func(t : T) { ks_(t); k(t) });
      };
      case (?t) { k(t) };
    };
  };

  (enqueue, fulfill)
};
|}
