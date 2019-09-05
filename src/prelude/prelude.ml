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
type Error = prim "Error";

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

func printNat(x : Nat) { print (@text_of_Nat x) };
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

func int64ToInt(n : Int64) : Int = (prim "num_conv_Int64_Int" : Int64 -> Int) n;
func intToInt64(n : Int) : Int64 = (prim "num_conv_Int_Int64" : Int -> Int64) n;
func int64ToWord64(n : Int64) : Word64 = (prim "num_conv_Int64_Word64" : Int64 -> Word64) n;
func word64ToInt64(n : Word64) : Int64 = (prim "num_conv_Word64_Int64" : Word64 -> Int64) n;
func int32ToInt(n : Int32) : Int = (prim "num_conv_Int32_Int" : Int32 -> Int) n;
func intToInt32(n : Int) : Int32 = (prim "num_conv_Int_Int32" : Int -> Int32) n;
func int32ToWord32(n : Int32) : Word32 = (prim "num_conv_Int32_Word32" : Int32 -> Word32) n;
func word32ToInt32(n : Word32) : Int32 = (prim "num_conv_Word32_Int32" : Word32 -> Int32) n;
func int16ToInt(n : Int16) : Int = (prim "num_conv_Int16_Int" : Int16 -> Int) n;
func intToInt16(n : Int) : Int16 = (prim "num_conv_Int_Int16" : Int -> Int16) n;
func int16ToWord16(n : Int16) : Word16 = (prim "num_conv_Int16_Word16" : Int16 -> Word16) n;
func word16ToInt16(n : Word16) : Int16 = (prim "num_conv_Word16_Int16" : Word16 -> Int16) n;
func int8ToInt(n : Int8) : Int = (prim "num_conv_Int8_Int" : Int8 -> Int) n;
func intToInt8(n : Int) : Int8 = (prim "num_conv_Int_Int8" : Int -> Int8) n;
func int8ToWord8(n : Int8) : Word8 = (prim "num_conv_Int8_Word8" : Int8 -> Word8) n;
func word8ToInt8(n : Word8) : Int8 = (prim "num_conv_Word8_Int8" : Word8 -> Int8) n;

func nat64ToNat(n : Nat64) : Nat = (prim "num_conv_Nat64_Nat" : Nat64 -> Nat) n;
func natToNat64(n : Nat) : Nat64 = (prim "num_conv_Nat_Nat64" : Nat -> Nat64) n;
func nat64ToWord64(n : Nat64) : Word64 = (prim "num_conv_Nat64_Word64" : Nat64 -> Word64) n;
func word64ToNat64(n : Word64) : Nat64 = (prim "num_conv_Word64_Nat64" : Word64 -> Nat64) n;
func nat32ToNat(n : Nat32) : Nat = (prim "num_conv_Nat32_Nat" : Nat32 -> Nat) n;
func natToNat32(n : Nat) : Nat32 = (prim "num_conv_Nat_Nat32" : Nat -> Nat32) n;
func nat32ToWord32(n : Nat32) : Word32 = (prim "num_conv_Nat32_Word32" : Nat32 -> Word32) n;
func word32ToNat32(n : Word32) : Nat32 = (prim "num_conv_Word32_Nat32" : Word32 -> Nat32) n;
func nat16ToNat(n : Nat16) : Nat = (prim "num_conv_Nat16_Nat" : Nat16 -> Nat) n;
func natToNat16(n : Nat) : Nat16 = (prim "num_conv_Nat_Nat16" : Nat -> Nat16) n;
func nat16ToWord16(n : Nat16) : Word16 = (prim "num_conv_Nat16_Word16" : Nat16 -> Word16) n;
func word16ToNat16(n : Word16) : Nat16 = (prim "num_conv_Word16_Nat16" : Word16 -> Nat16) n;
func nat8ToNat(n : Nat8) : Nat = (prim "num_conv_Nat8_Nat" : Nat8 -> Nat) n;
func natToNat8(n : Nat) : Nat8 = (prim "num_conv_Nat_Nat8" : Nat -> Nat8) n;
func nat8ToWord8(n : Nat8) : Word8 = (prim "num_conv_Nat8_Word8" : Nat8 -> Word8) n;
func word8ToNat8(n : Word8) : Nat8 = (prim "num_conv_Word8_Nat8" : Word8 -> Nat8) n;


func natToWord8(n : Nat) : Word8 = (prim "num_conv_Nat_Word8" : Nat -> Word8) n;
func word8ToNat(n : Word8) : Nat = (prim "num_conv_Word8_Nat" : Word8 -> Nat) n;
func intToWord8(n : Int) : Word8 = (prim "num_conv_Int_Word8" : Int -> Word8) n;
func word8ToInt(n : Word8) : Int = (prim "num_conv_Word8_Int" : Word8 -> Int) n;

func natToWord16(n : Nat) : Word16 = (prim "num_conv_Nat_Word16" : Nat -> Word16) n;
func word16ToNat(n : Word16) : Nat = (prim "num_conv_Word16_Nat" : Word16 -> Nat) n;
func intToWord16(n : Int) : Word16 = (prim "num_conv_Int_Word16" : Int -> Word16) n;
func word16ToInt(n : Word16) : Int = (prim "num_conv_Word16_Int" : Word16 -> Int) n;

func natToWord32(n : Nat) : Word32 = (prim "num_conv_Nat_Word32" : Nat -> Word32) n;
func word32ToNat(n : Word32) : Nat = (prim "num_conv_Word32_Nat" : Word32 -> Nat) n;
func intToWord32(n : Int) : Word32 = (prim "num_conv_Int_Word32" : Int -> Word32) n;
func word32ToInt(n : Word32) : Int = (prim "num_conv_Word32_Int" : Word32 -> Int) n;

func natToWord64(n : Nat) : Word64 = (prim "num_conv_Nat_Word64" : Nat -> Word64) n;
func word64ToNat(n : Word64) : Nat = (prim "num_conv_Word64_Nat" : Word64 -> Nat) n;
func intToWord64(n : Int) : Word64 = (prim "num_conv_Int_Word64" : Int -> Word64) n;
func word64ToInt(n : Word64) : Int = (prim "num_conv_Word64_Int" : Word64 -> Int) n;

func charToWord32(c : Char) : Word32 = (prim "num_conv_Char_Word32" : Char -> Word32) c;
func word32ToChar(w : Word32) : Char = (prim "num_conv_Word32_Char" : Word32 -> Char) w;

func charToText(c : Char) : Text = (prim "conv_Char_Text" : Char -> Text) c;

// Exotic bitwise operations
func popcntWord8(w : Word8) : Word8 = (prim "popcnt8" : Word8 -> Word8) w;
func clzWord8(w : Word8) : Word8 = (prim "clz8" : Word8 -> Word8) w;
func ctzWord8(w : Word8) : Word8 = (prim "ctz8" : Word8 -> Word8) w;
func btstWord8(w : Word8, amount : Word8) : Bool = (prim "btst8" : (Word8, Word8) -> Word8) (w, amount) != (0 : Word8);

func popcntWord16(w : Word16) : Word16 = (prim "popcnt16" : Word16 -> Word16) w;
func clzWord16(w : Word16) : Word16 = (prim "clz16" : Word16 -> Word16) w;
func ctzWord16(w : Word16) : Word16 = (prim "ctz16" : Word16 -> Word16) w;
func btstWord16(w : Word16, amount : Word16) : Bool = (prim "btst16" : (Word16, Word16) -> Word16) (w, amount) != (0 : Word16);

func popcntWord32(w : Word32) : Word32 = (prim "popcnt32" : Word32 -> Word32) w;
func clzWord32(w : Word32) : Word32 = (prim "clz32" : Word32 -> Word32) w;
func ctzWord32(w : Word32) : Word32 = (prim "ctz32" : Word32 -> Word32) w;
func btstWord32(w : Word32, amount : Word32) : Bool = (prim "btst32" : (Word32, Word32) -> Word32) (w, amount) != (0 : Word32);

func popcntWord64(w : Word64) : Word64 = (prim "popcnt64" : Word64 -> Word64) w;
func clzWord64(w : Word64) : Word64 = (prim "clz64" : Word64 -> Word64) w;
func ctzWord64(w : Word64) : Word64 = (prim "ctz64" : Word64 -> Word64) w;
func btstWord64(w : Word64, amount : Word64) : Bool = (prim "btst64" : (Word64, Word64) -> Word64) (w, amount) != (0 : Word64);


// Internal helper functions for the show translation

// The @ in the name ensures that this cannot be shadowed by user code, so
// compiler passes can rely on them being in scope
// The text_of functions do not need to be exposed; the user can just use
// the show above.

func @text_of_num(x : Nat, base : Nat, sep : Nat, digits : [Text]) : Text {
  var text = "";
  var n = x;

  if (n == 0) return "0";

  var i = 0;
  while (n > 0) {
    let rem = n % base;
    if (i == sep) { text := "_" # text; i := 0 };
    text := digits[rem] # text;
    n := n / base;
    i += 1;
  };
  return text;
};

let @decdigits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
func @text_of_Nat(x : Nat) : Text {
  @text_of_num(x, 10, 3, @decdigits);
};

func @text_of_Int(x : Int) : Text {
  if (x == 0) "0" else (if (x < 0) "-" else "+") # @text_of_Nat(abs x)
};

let @hexdigits =
  [ "0", "1", "2", "3", "4", "5", "6", "7",
    "8", "9", "A", "B", "C", "D", "E", "F" ];
func @text_of_Word(x : Nat) : Text {
  return "0x" # @text_of_num(x, 16, 4, @hexdigits);
};

func @text_of_Nat8(x : Nat8) : Text = @text_of_Nat (nat8ToNat x);
func @text_of_Nat16(x : Nat16) : Text = @text_of_Nat (nat16ToNat x);
func @text_of_Nat32(x : Nat32) : Text = @text_of_Nat (nat32ToNat x);
func @text_of_Nat64(x : Nat64) : Text = @text_of_Nat (nat64ToNat x);
func @text_of_Int8(x : Int8) : Text = @text_of_Int (int8ToInt x);
func @text_of_Int16(x : Int16) : Text = @text_of_Int (int16ToInt x);
func @text_of_Int32(x : Int32) : Text = @text_of_Int (int32ToInt x);
func @text_of_Int64(x : Int64) : Text = @text_of_Int (int64ToInt x);
func @text_of_Word8(x : Word8) : Text = @text_of_Word (word8ToNat x);
func @text_of_Word16(x : Word16) : Text = @text_of_Word (word16ToNat x);
func @text_of_Word32(x : Word32) : Text = @text_of_Word (word32ToNat x);
func @text_of_Word64(x : Word64) : Text = @text_of_Word (word64ToNat x);


func @text_of_Bool(b : Bool) : Text {
  if (b) "true" else "false"
};

func @text_of_Text(t : Text) : Text {
  // TODO: Escape properly
  "\"" # t # "\"";
};

func @text_has_parens(t : Text) : Bool {
  switch (t.chars().next()) {
    case (?'(') true;
    case _ false;
  }
};

func @text_needs_parens(t : Text) : Bool {
  switch (t.chars().next()) {
    case (?('+' or '-' or '?' or '#')) true;
    case _ false;
  }
};

func @text_of_option<T>(f : T -> Text, x : ?T) : Text {
  switch (x) {
    case (?y) {
      let fy = f y;
      if (@text_needs_parens(fy)) "?(" # fy # ")"
      else "?" # fy
    };
    case null {"null"};
  }
};

func @text_of_variant<T>(l : Text, f : T -> Text, x : T) : Text {
  let fx = f x;
  if (fx == "()") "#" # l
  else if (@text_has_parens(fx)) "#" # l # fx
  else "#" # l # "(" # fx # ")"
};

func @text_of_array<T>(f : T -> Text, xs : [T]) : Text {
  var text = "[";
  var first = true;
  for (x in xs.vals()) {
    if first {
      first := false;
    } else {
      text #= ", ";
    };
    text #= f x;
  };
  text #= "]";
  return text;
};

func @text_of_array_mut<T>(f : T -> Text, xs : [var T]) : Text {
  var text = "[var";
  var first = true;
  for (x in xs.vals()) {
    if first {
      first := false;
      text #= " ";
    } else {
      text #= ", ";
    };
    text #= f x;
  };
  text #= "]";
  return text;
};


// Array utilities

// TODO: These would be nicer as a objects, but lets do them as functions
// until the compiler has a concept of “static objects”
func Array_init<T>(len : Nat,  x : T) : [var T] {
  (prim "Array.init" : <T>(Nat, T) -> [var T])<T>(len, x)
};

func Array_tabulate<T>(len : Nat,  gen : Nat -> T) : [T] {
  (prim "Array.tabulate" : <T>(Nat, Nat -> T) -> [T])<T>(len, gen)
};

// these will change
type ErrorCode = {#error; #system};

func error(message : Text) : Error =
  (prim "error" : Text -> Error)(message);

func errorCode(e : Error) : ErrorCode =
  (prim "errorCode" : Error -> ErrorCode)(e);

func errorMessage(e : Error) : Text =
  (prim "errorMessage" : Error -> Text)(e);

type Cont<T> = T -> () ;
type Async<T> = (Cont<T>,Cont<Error>) -> ();

type Result<T> = {#ok : T; #error : Error};

func @new_async<T <: Any>() : (Async<T>, Cont<T>, Cont<Error>) {
  let k_null = func(_ : T) {};
  let r_null = func(_ : Error) {};
  var result : ?(Result<T>) = null;
  var ks : Cont<T> = k_null;
  var rs : Cont<Error> = r_null;

  func fulfill(t : T) {
    switch result {
      case null {
        result := ?(#ok t);
        let ks_ = ks;
        ks := k_null;
        rs := r_null;
        ks_(t);
      };
      case (? _) { assert false };
    };
  };

  func fail(e : Error) {
    switch result {
      case null {
        result := ?(#error e);
        let rs_ = rs;
        ks := k_null;
        rs := r_null;
        rs_(e);
      };
      case (? _) { assert false };
    };
  };

  func enqueue(k : Cont<T>, r : Cont<Error>) {
    switch result {
      case null {
        let ks_ = ks;
        ks := (func(t : T) { ks_(t); k(t) });
        let rs_ = rs;
        rs := (func(e : Error) { rs_(e); r(e) });
      };
      case (? (#ok t)) { k(t) };
      case (? (#error e)) { r(e) };
    };
  };

  (enqueue, fulfill, fail)
};
|}
