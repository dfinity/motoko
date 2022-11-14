(*
let prelude = {|

/*
The prelude. This stuff is always initially in scope. This should be only
 * type definitions for primitive types
 * code with privileged names (@…) that are used by the desugarer or ir-passes
*/

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
type Blob = prim "Blob";
type Error = prim "Error";
type Principal = prim "Principal";

type @Iter<T_> = {next : () -> ?T_};

var @cycles : Nat64 = 0;

// Function called by backend to add funds to call.
// DO NOT RENAME without modifying compilation.
func @add_cycles() {
  let cycles = @cycles;
  @reset_cycles();
  (prim "cyclesAdd" : (Nat64) -> ()) (cycles);
};

// Function called by backend to zero cycles on context switch.
// DO NOT RENAME without modifying compilation.
func @reset_cycles() {
  @cycles := 0;
};


// The @ in the name ensures that this cannot be shadowed by user code, so
// compiler passes can rely on them being in scope

// Implementations for overloaded dot operations
// Note that these return functions!
// (Some optimizations in the backend might be feasible.)

func @immut_array_get<A>(xs : [A]) : Nat -> A =
  func (n : Nat) : A = xs[n];
func @mut_array_get<A>(xs : [var A]) : Nat -> A =
  func (n : Nat) : A = xs[n];
func @immut_array_size<A>(xs : [A]) : () -> Nat =
  func () : Nat = (prim "array_len" : [A] -> Nat) xs;
func @mut_array_size<A>(xs : [var A]) : () -> Nat =
  func () : Nat = (prim "array_len" : [var A] -> Nat) xs;
func @mut_array_put<A>(xs : [var A]) : (Nat, A) -> () =
  func (n : Nat, x : A) = (xs[n] := x);
func @immut_array_keys<A>(xs : [A]) : () -> @Iter<Nat> =
  func () : @Iter<Nat> = object {
    var i = 0;
    let l = xs.size();
    public func next() : ?Nat { if (i >= l) null else {let j = i; i += 1; ?j} };
  };
func @mut_array_keys<A>(xs : [var A]) : () -> @Iter<Nat> =
  func () : @Iter<Nat> = object {
    var i = 0;
    let l = xs.size();
    public func next() : ?Nat { if (i >= l) null else {let j = i; i += 1; ?j} };
  };
func @immut_array_vals<A>(xs : [A]) : () -> @Iter<A> =
  func () : @Iter<A> = object {
    var i = 0;
    let l = xs.size();
    public func next() : ?A { if (i >= l) null else {let j = i; i += 1; ?xs[j]} };
  };
func @mut_array_vals<A>(xs : [var A]) : () -> @Iter<A> =
  func () : @Iter<A> = object {
    var i = 0;
    let l = xs.size();
    public func next() : ?A { if (i >= l) null else {let j = i; i += 1; ?xs[j]} };
  };
func @blob_size(xs : Blob) : () -> Nat =
  func () : Nat = (prim "blob_size" : Blob -> Nat) xs;
func @blob_bytes(xs : Blob) : () -> @Iter<Word8> =
  func () : @Iter<Word8> = object {
    type BlobIter = Any; // not exposed
    let i = (prim "blob_iter" : Blob -> BlobIter) xs;
    public func next() : ?Word8 {
      if ((prim "blob_iter_done" : BlobIter -> Bool) i)
        null
      else
        ?((prim "blob_iter_next" : BlobIter -> Word8) i)
    };
  };
func @text_size(xs : Text) : () -> Nat =
  func () : Nat = (prim "text_len" : Text -> Nat) xs;
func @text_chars(xs : Text) : () -> @Iter<Char> =
  func () : @Iter<Char> = object {
    type TextIter = Any; // not exposed
    let i = (prim "text_iter" : Text -> TextIter) xs;
    public func next() : ?Char {
      if ((prim "text_iter_done" : TextIter -> Bool) i)
        null
      else
        ?((prim "text_iter_next" : TextIter -> Char) i)
    };
  };


// Internal helper functions for the show translation

// The text_of functions do not need to be exposed; the user can just use
// the show above.

func @text_of_num(x : Nat, base : Nat, sep : Nat, digits : Nat -> Text) : Text {
  var text = "";
  var n = x;

  if (n == 0) return "0";

  var i = 0;
  while (n > 0) {
    let rem = n % base;
    if (sep > 0 and i == sep) { text := "_" # text; i := 0 };
    text := digits rem # text;
    n := n / base;
    i += 1;
  };
  text
};

func @left_pad(pad : Nat, char : Text, t : Text) : Text {
  if (pad > t.size()) {
    var i = pad - t.size();
    var text = t;
    while (i > 0) {
      text := char # text;
      i -= 1;
    };
    text
  } else {
    t
  }
};

func @digits_dec(x : Nat) : Text =
 (prim "conv_Char_Text" : Char -> Text) (
   (prim "num_conv_Word32_Char" : Word32 -> Char) (
     (prim "num_conv_Nat_Word32" : Nat -> Word32) (
       x + 0x30
     )
   )
 );

func @text_of_Nat(x : Nat) : Text {
  @text_of_num(x, 10, 3, @digits_dec);
};

func @text_of_Int(x : Int) : Text {
  if (x == 0) "0" else (if (x < 0) "-" else "+") #
  @text_of_Nat((prim "abs" : Int -> Nat) x)
};

func @digits_hex(x : Nat) : Text =
 (prim "conv_Char_Text" : Char -> Text) (
   (prim "num_conv_Word32_Char" : Word32 -> Char) (
     (prim "num_conv_Nat_Word32" : Nat -> Word32) (
       x + (if (x < 10) 0x30 else 55)
     )
   )
 );

func @text_of_Word(x : Nat) : Text = "0x" # @text_of_num(x, 16, 4, @digits_hex);

// There is some duplication with the prim_module, but we need these here
// before we can load the prim module
func @int64ToInt(n : Int64) : Int = (prim "num_conv_Int64_Int" : Int64 -> Int) n;
func @int32ToInt(n : Int32) : Int = (prim "num_conv_Int32_Int" : Int32 -> Int) n;
func @int16ToInt(n : Int16) : Int = (prim "num_conv_Int16_Int" : Int16 -> Int) n;
func @int8ToInt(n : Int8) : Int = (prim "num_conv_Int8_Int" : Int8 -> Int) n;
func @nat64ToNat(n : Nat64) : Nat = (prim "num_conv_Nat64_Nat" : Nat64 -> Nat) n;
func @nat32ToNat(n : Nat32) : Nat = (prim "num_conv_Nat32_Nat" : Nat32 -> Nat) n;
func @nat16ToNat(n : Nat16) : Nat = (prim "num_conv_Nat16_Nat" : Nat16 -> Nat) n;
func @nat8ToNat(n : Nat8) : Nat = (prim "num_conv_Nat8_Nat" : Nat8 -> Nat) n;
func @word64ToNat(n : Word64) : Nat = (prim "num_conv_Word64_Nat" : Word64 -> Nat) n;
func @word32ToNat(n : Word32) : Nat = (prim "num_conv_Word32_Nat" : Word32 -> Nat) n;
func @word16ToNat(n : Word16) : Nat = (prim "num_conv_Word16_Nat" : Word16 -> Nat) n;
func @word8ToNat(n : Word8) : Nat = (prim "num_conv_Word8_Nat" : Word8 -> Nat) n;

func @text_of_Nat8(x : Nat8) : Text = @text_of_Nat (@nat8ToNat x);
func @text_of_Nat16(x : Nat16) : Text = @text_of_Nat (@nat16ToNat x);
func @text_of_Nat32(x : Nat32) : Text = @text_of_Nat (@nat32ToNat x);
func @text_of_Nat64(x : Nat64) : Text = @text_of_Nat (@nat64ToNat x);
func @text_of_Int8(x : Int8) : Text = @text_of_Int (@int8ToInt x);
func @text_of_Int16(x : Int16) : Text = @text_of_Int (@int16ToInt x);
func @text_of_Int32(x : Int32) : Text = @text_of_Int (@int32ToInt x);
func @text_of_Int64(x : Int64) : Text = @text_of_Int (@int64ToInt x);
func @text_of_Word8(x : Word8) : Text = @text_of_Word (@word8ToNat x);
func @text_of_Word16(x : Word16) : Text = @text_of_Word (@word16ToNat x);
func @text_of_Word32(x : Word32) : Text = @text_of_Word (@word32ToNat x);
func @text_of_Word64(x : Word64) : Text = @text_of_Word (@word64ToNat x);
func @text_of_Float(x : Float) : Text = (prim "Float->Text" : Float -> Text) x;


func @text_of_Bool(b : Bool) : Text {
  if (b) "true" else "false"
};

func @text_of_Text(t : Text) : Text {
  // TODO: Escape properly
  "\"" # t # "\"";
};

func @text_of_Char(c : Char) : Text {
  // TODO: Escape properly
  "\'" # (prim "conv_Char_Text" : Char -> Text) c # "\'";
};

func @text_of_Blob(blob : Blob) : Text {
  var t = "\"";
  for (b in blob.bytes()) {
    // Could do more clever escaping, e.g. leave ascii and utf8 in place
    t #= "\\" # @left_pad(2, "0", @text_of_num(@word8ToNat b, 16, 0, @digits_hex));
  };
  t #= "\"";
  return t;
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
  text # "]"
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
  text # "]"
};

func @equal_array<T>(eq : (T, T) -> Bool, a : [T], b : [T]) : Bool {
  if (a.size() != b.size()) {
    return false;
  };
  var i = 0;
  let s = a.size();
  while (i < s) {
    if (not eq(a[i],b[i])) {
      return false;
    };
    i += 1;
  };
  return true;
};

type @Cont<T> = T -> () ;
type @Async<T> = (@Cont<T>,@Cont<Error>) -> ();

type @Refund = Nat64;
type @Result<T> = {#ok : (refund : @Refund, value: T); #error : Error};

type @Waiter<T> = (@Refund,T) -> () ;

var @refund : @Refund = 0;

// Function called by backend to zero refunds on context switch.
// DO NOT RENAME without modifying compilation.
func @reset_refund() {
  @refund := 0;
};

func @getSystemRefund() : @Refund {
  return (prim "cyclesRefunded" : () -> Nat64) ();
};

func @new_async<T <: Any>(getSystemRefund : Bool) : (@Async<T>, @Cont<T>, @Cont<Error>) {
  let w_null = func(r : @Refund, t : T) { };
  let r_null = func(_ : Error) {};
  var result : ?(@Result<T>) = null;
  var ws : @Waiter<T> = w_null;
  var rs : @Cont<Error> = r_null;

  func fulfill(t : T) {
    switch result {
      case null {
        let refund = if (getSystemRefund) @getSystemRefund() else @refund;
        result := ?(#ok (refund, t));
        let ws_ = ws;
        ws := w_null;
        rs := r_null;
        ws_(refund, t);
      };
      case (? _) { assert false };
    };
  };

  func fail(e : Error) {
    switch result {
      case null {
        result := ?(#error e);
        let rs_ = rs;
        ws := w_null;
        rs := r_null;
        rs_(e);
      };
      case (? _) { assert false };
    };
  };

  func enqueue(k : @Cont<T>, r : @Cont<Error>) {
    switch result {
      case null {
        let ws_ = ws;
        ws := func(r : @Refund, t : T) {
          ws_(r, t);
          @reset_cycles();
          @refund := r;
          k(t);
        };
        let rs_ = rs;
        rs := func(e : Error) {
          rs_(e);
          @reset_cycles();
          @reset_refund();
          r(e) };
      };
      case (? (#ok (r, t))) {
        @refund := r;
        k(t)
      };
      case (? (#error e)) {
        r(e)
      };
    };
  };

  (enqueue, fulfill, fail)
};

let @ic00 = actor "aaaaa-aa" : actor {
  create_canister : () -> async { canister_id : Principal };
  install_code : {
    mode : { #install; #reinstall; #upgrade };
    canister_id : Principal;
    wasm_module : Blob;
    arg : Blob;
    compute_allocation : ?Nat;
    memory_allocation : ?Nat;
  } -> async ()
};

// uses `do async {}`, not `async { }`, to avoid initial context switch
func @create_actor_helper(wasm_module_ : Blob, arg_ : Blob) : async Principal = do async {
  let available = (prim "cyclesAvailable" : () -> Nat64) ();
  let accepted = (prim "cyclesAccept" : Nat64 -> Nat64) (available);
  @cycles += accepted;
  let { canister_id = canister_id_ } =
    await @ic00.create_canister();
  await @ic00.install_code({
    mode = #install;
    canister_id = canister_id_;
    wasm_module = wasm_module_;
    arg = arg_;
    compute_allocation = null;
    memory_allocation = null;
  });
  return canister_id_;
};

|}

(*
The primitive definitions.

This module should contain everything that cannot be implemented in plain
Motoko. It is available via `import Prim "mo:prim"`. Normal user code would
usually not import that module directly, but through base, which takes
care of providing a proper module structure, e.g. exposing Array_tabulate
through Array.tabulate.
*)
let prim_module =
{|

func abs(x : Int) : Nat { (prim "abs" : Int -> Nat) x };

// for testing
func idlHash(x : Text) : Word32 { (prim "idlHash" : Text -> Word32) x };

// Printing

func debugPrint(x : Text) { (prim "print" : Text -> ()) x };
func debugPrintNat(x : Nat) { debugPrint (@text_of_Nat x) };
func debugPrintInt(x : Int) { debugPrint (@text_of_Int x) };
func debugPrintChar(x : Char) { debugPrint (charToText x) };

// RTS stats

func rts_version() : Text { (prim "rts_version" : () -> Text) () };
func rts_memory_size() : Nat { (prim "rts_memory_size" : () -> Nat) () };
func rts_heap_size() : Nat { (prim "rts_heap_size" : () -> Nat) () };
func rts_total_allocation() : Nat { (prim "rts_total_allocation" : () -> Nat) () };
func rts_reclaimed() : Nat { (prim "rts_reclaimed" : () -> Nat) () };
func rts_max_live_size() : Nat { (prim "rts_max_live_size" : () -> Nat) () };
func rts_callback_table_count() : Nat { (prim "rts_callback_table_count" : () -> Nat) () };
func rts_callback_table_size() : Nat { (prim "rts_callback_table_size" : () -> Nat) () };

// Hashing

func hashBlob(b : Blob) : Word32 { (prim "crc32Hash" : Blob -> Word32) b };

// Conversions

let int64ToInt = @int64ToInt;
let int32ToInt = @int32ToInt;
let int16ToInt = @int16ToInt;
let int8ToInt = @int8ToInt;
let nat64ToNat = @nat64ToNat;
let nat32ToNat = @nat32ToNat;
let nat16ToNat = @nat16ToNat;
let nat8ToNat = @nat8ToNat;
let word64ToNat = @word64ToNat;
let word32ToNat = @word32ToNat;
let word16ToNat = @word16ToNat;
let word8ToNat = @word8ToNat;

func intToInt64(n : Int) : Int64 = (prim "num_conv_Int_Int64" : Int -> Int64) n;
func int64ToWord64(n : Int64) : Word64 = (prim "num_conv_Int64_Word64" : Int64 -> Word64) n;
func word64ToInt64(n : Word64) : Int64 = (prim "num_conv_Word64_Int64" : Word64 -> Int64) n;
func intToInt32(n : Int) : Int32 = (prim "num_conv_Int_Int32" : Int -> Int32) n;
func int32ToWord32(n : Int32) : Word32 = (prim "num_conv_Int32_Word32" : Int32 -> Word32) n;
func word32ToInt32(n : Word32) : Int32 = (prim "num_conv_Word32_Int32" : Word32 -> Int32) n;
func intToInt16(n : Int) : Int16 = (prim "num_conv_Int_Int16" : Int -> Int16) n;
func int16ToWord16(n : Int16) : Word16 = (prim "num_conv_Int16_Word16" : Int16 -> Word16) n;
func word16ToInt16(n : Word16) : Int16 = (prim "num_conv_Word16_Int16" : Word16 -> Int16) n;
func intToInt8(n : Int) : Int8 = (prim "num_conv_Int_Int8" : Int -> Int8) n;
func int8ToWord8(n : Int8) : Word8 = (prim "num_conv_Int8_Word8" : Int8 -> Word8) n;
func word8ToInt8(n : Word8) : Int8 = (prim "num_conv_Word8_Int8" : Word8 -> Int8) n;

func natToNat64(n : Nat) : Nat64 = (prim "num_conv_Nat_Nat64" : Nat -> Nat64) n;
func nat64ToWord64(n : Nat64) : Word64 = (prim "num_conv_Nat64_Word64" : Nat64 -> Word64) n;
func word64ToNat64(n : Word64) : Nat64 = (prim "num_conv_Word64_Nat64" : Word64 -> Nat64) n;
func natToNat32(n : Nat) : Nat32 = (prim "num_conv_Nat_Nat32" : Nat -> Nat32) n;
func nat32ToWord32(n : Nat32) : Word32 = (prim "num_conv_Nat32_Word32" : Nat32 -> Word32) n;
func word32ToNat32(n : Word32) : Nat32 = (prim "num_conv_Word32_Nat32" : Word32 -> Nat32) n;
func natToNat16(n : Nat) : Nat16 = (prim "num_conv_Nat_Nat16" : Nat -> Nat16) n;
func nat16ToWord16(n : Nat16) : Word16 = (prim "num_conv_Nat16_Word16" : Nat16 -> Word16) n;
func word16ToNat16(n : Word16) : Nat16 = (prim "num_conv_Word16_Nat16" : Word16 -> Nat16) n;
func natToNat8(n : Nat) : Nat8 = (prim "num_conv_Nat_Nat8" : Nat -> Nat8) n;
func nat8ToWord8(n : Nat8) : Word8 = (prim "num_conv_Nat8_Word8" : Nat8 -> Word8) n;
func word8ToNat8(n : Word8) : Nat8 = (prim "num_conv_Word8_Nat8" : Word8 -> Nat8) n;

func natToWord8(n : Nat) : Word8 = (prim "num_conv_Nat_Word8" : Nat -> Word8) n;
func intToWord8(n : Int) : Word8 = (prim "num_conv_Int_Word8" : Int -> Word8) n;
func natToWord16(n : Nat) : Word16 = (prim "num_conv_Nat_Word16" : Nat -> Word16) n;
func intToWord16(n : Int) : Word16 = (prim "num_conv_Int_Word16" : Int -> Word16) n;
func natToWord32(n : Nat) : Word32 = (prim "num_conv_Nat_Word32" : Nat -> Word32) n;
func intToWord32(n : Int) : Word32 = (prim "num_conv_Int_Word32" : Int -> Word32) n;
func natToWord64(n : Nat) : Word64 = (prim "num_conv_Nat_Word64" : Nat -> Word64) n;
func intToWord64(n : Int) : Word64 = (prim "num_conv_Int_Word64" : Int -> Word64) n;

func charToWord32(c : Char) : Word32 = (prim "num_conv_Char_Word32" : Char -> Word32) c;
func word32ToChar(w : Word32) : Char = (prim "num_conv_Word32_Char" : Word32 -> Char) w;

func charToText(c : Char) : Text = (prim "conv_Char_Text" : Char -> Text) c;

func charToUpper(c : Char) : Char = (prim "char_to_upper" : Char -> Char) c;
func charToLower(c : Char) : Char = (prim "char_to_lower" : Char -> Char) c;
func charIsWhitespace(c : Char) : Bool = (prim "char_is_whitespace" : Char -> Bool) c;
func charIsLowercase(c : Char) : Bool = (prim "char_is_lowercase" : Char -> Bool) c;
func charIsUppercase(c : Char) : Bool = (prim "char_is_uppercase" : Char -> Bool) c;
func charIsAlphabetic(c : Char) : Bool = (prim "char_is_alphabetic" : Char -> Bool) c;

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

// Float operations

func floatAbs(f : Float) : Float = (prim "fabs" : Float -> Float) f;
func floatSqrt(f : Float) : Float = (prim "fsqrt" : Float -> Float) f;
func floatCeil(f : Float) : Float = (prim "fceil" : Float -> Float) f;
func floatFloor(f : Float) : Float = (prim "ffloor" : Float -> Float) f;
func floatTrunc(f : Float) : Float = (prim "ftrunc" : Float -> Float) f;
func floatNearest(f : Float) : Float = (prim "fnearest" : Float -> Float) f;
func floatMin(f : Float, g : Float) : Float = (prim "fmin" : (Float, Float) -> Float) (f, g);
func floatMax(f : Float, g : Float) : Float = (prim "fmax" : (Float, Float) -> Float) (f, g);
func floatCopySign(f : Float, g : Float) : Float = (prim "fcopysign" : (Float, Float) -> Float) (f, g);
func floatToInt64(f : Float) : Int64 = (prim "num_conv_Float_Int64" : Float -> Int64) (f);
func int64ToFloat(n : Int64) : Float = (prim "num_conv_Int64_Float" : Int64 -> Float) (n);

let floatToText = @text_of_Float;

// Configurable Float formatter
// mode:
//  0) fixed format "%.*f"
//  1) exponent format "%.*e"
//  2) generic format "%.*g"
//  3) hexadecimal format "%.*h"
//  _) invalid (traps)
func floatToFormattedText(f : Float, prec : Nat8, mode : Nat8) : Text = (prim "fmtFloat->Text" : (Float, Nat8, Nat8) -> Text) (f, prec, mode);

// Trigonometric and transcendental functions

func sin(f : Float) : Float = (prim "fsin" : Float -> Float) f;
func cos(f : Float) : Float = (prim "fcos" : Float -> Float) f;
func tan(f : Float) : Float = (prim "ftan" : Float -> Float) f;
func arcsin(f : Float) : Float = (prim "fasin" : Float -> Float) f;
func arccos(f : Float) : Float = (prim "facos" : Float -> Float) f;
func arctan(f : Float) : Float = (prim "fatan" : Float -> Float) f;
func arctan2(y : Float, x : Float) : Float = (prim "fatan2" : (Float, Float) -> Float) (y, x);

func exp(f : Float) : Float = (prim "fexp" : Float -> Float) f;
func log(f : Float) : Float = (prim "flog" : Float -> Float) f;

// Array utilities

func Array_init<T>(len : Nat,  x : T) : [var T] {
  (prim "Array.init" : <T>(Nat, T) -> [var T])<T>(len, x)
};

func Array_tabulate<T>(len : Nat,  gen : Nat -> T) : [T] {
  (prim "Array.tabulate" : <T>(Nat, Nat -> T) -> [T])<T>(len, gen)
};

// Error codes
type ErrorCode = {
  #system_fatal;
  #system_transient;
  #destination_invalid;
  #canister_reject;
  #canister_error;
  #future : Nat32;
};

// creation and inspection of abstract error
func error(message : Text) : Error {
  let e = (#canister_reject, message);
  (prim "cast" : (ErrorCode, Text) -> Error) e
};
func errorCode(e : Error) : ErrorCode =
  ((prim "cast" : Error -> (ErrorCode, Text)) e).0;
func errorMessage(e : Error) : Text =
  ((prim "cast" : Error -> (ErrorCode, Text)) e).1;

// Time

func time() : Nat64 = (prim "time" : () -> Nat64) ();

// Principal

func blobOfPrincipal(id : Principal) : Blob = (prim "cast" : Principal -> Blob) id;

func principalOfActor(act : actor {}) : Principal = (prim "cast" : (actor {}) -> Principal) act;

// Untyped dynamic actor creation from blobs
let createActor : (wasm : Blob, argument : Blob) -> async Principal = @create_actor_helper;

func cyclesBalance() : Nat64 {
  (prim "cyclesBalance" : () -> Nat64) ();
};

func cyclesAvailable() : Nat64 {
  (prim "cyclesAvailable" : () -> Nat64) ();
};

func cyclesRefunded() : Nat64 {
    @refund
};

func cyclesAccept(amount: Nat64) : Nat64 {
  (prim "cyclesAccept" : Nat64 -> Nat64) (amount);
};

func cyclesAdd(amount: Nat64) : () {
  @cycles += amount;
};


|}
*)
let prelude = [%blob "prelude.mo"]
let internals = [%blob "internals.mo"]
let prim_module = [%blob "prim.mo"]

