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
func @blob_bytes(xs : Blob) : () -> @Iter<Nat8> =
  func () : @Iter<Nat8> = object {
    type BlobIter = Any; // not exposed
    let i = (prim "blob_iter" : Blob -> BlobIter) xs;
    public func next() : ?Nat8 {
      if ((prim "blob_iter_done" : BlobIter -> Bool) i)
        null
      else
        ?((prim "blob_iter_next" : BlobIter -> Nat8) i)
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
   (prim "num_conv_Nat32_Char" : Nat32 -> Char) (
     (prim "num_conv_Nat_Nat32" : Nat -> Nat32) (
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
   (prim "num_conv_Nat32_Char" : Nat32 -> Char) (
     (prim "num_conv_Nat_Nat32" : Nat -> Nat32) (
       x + (if (x < 10) 0x30 else 55)
     )
   )
 );

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

func @text_of_Nat8(x : Nat8) : Text = @text_of_Nat (@nat8ToNat x);
func @text_of_Nat16(x : Nat16) : Text = @text_of_Nat (@nat16ToNat x);
func @text_of_Nat32(x : Nat32) : Text = @text_of_Nat (@nat32ToNat x);
func @text_of_Nat64(x : Nat64) : Text = @text_of_Nat (@nat64ToNat x);
func @text_of_Int8(x : Int8) : Text = @text_of_Int (@int8ToInt x);
func @text_of_Int16(x : Int16) : Text = @text_of_Int (@int16ToInt x);
func @text_of_Int32(x : Int32) : Text = @text_of_Int (@int32ToInt x);
func @text_of_Int64(x : Int64) : Text = @text_of_Int (@int64ToInt x);
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
    t #= "\\" # @left_pad(2, "0", @text_of_num(@nat8ToNat b, 16, 0, @digits_hex));
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

func @new_async<T <: Any>() : (@Async<T>, @Cont<T>, @Cont<Error>) {
  let w_null = func(r : @Refund, t : T) { };
  let r_null = func(_ : Error) {};
  var result : ?(@Result<T>) = null;
  var ws : @Waiter<T> = w_null;
  var rs : @Cont<Error> = r_null;

  func fulfill(t : T) {
    switch result {
      case null {
        let refund = @getSystemRefund();
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

// It would be desirable if create_actor_helper can be defined
// without paying the extra self-remote-call-cost
func @create_actor_helper(wasm_module_ : Blob, arg_ : Blob) : async Principal = async {
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
func idlHash(x : Text) : Nat32 { (prim "idlHash" : Text -> Nat32) x };

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

func hashBlob(b : Blob) : Nat32 { (prim "crc32Hash" : Blob -> Nat32) b };

// Total conversions (fixed to big)

let int64ToInt = @int64ToInt;
let int32ToInt = @int32ToInt;
let int16ToInt = @int16ToInt;
let int8ToInt = @int8ToInt;
let nat64ToNat = @nat64ToNat;
let nat32ToNat = @nat32ToNat;
let nat16ToNat = @nat16ToNat;
let nat8ToNat = @nat8ToNat;

// Trapping conversions (big to fixed)

// Trapping conversions (big to fixed)

func intToInt64(n : Int) : Int64 = (prim "num_conv_Int_Int64" : Int -> Int64) n;
func intToInt32(n : Int) : Int32 = (prim "num_conv_Int_Int32" : Int -> Int32) n;
func intToInt16(n : Int) : Int16 = (prim "num_conv_Int_Int16" : Int -> Int16) n;
func intToInt8(n : Int) : Int8 = (prim "num_conv_Int_Int8" : Int -> Int8) n;

func natToNat64(n : Nat) : Nat64 = (prim "num_conv_Nat_Nat64" : Nat -> Nat64) n;
func natToNat32(n : Nat) : Nat32 = (prim "num_conv_Nat_Nat32" : Nat -> Nat32) n;
func natToNat16(n : Nat) : Nat16 = (prim "num_conv_Nat_Nat16" : Nat -> Nat16) n;
func natToNat8(n : Nat) : Nat8 = (prim "num_conv_Nat_Nat8" : Nat -> Nat8) n;

// Wrapping conversions (big to fixed, and within fixed)

func intToNat64Wrap(n : Int) : Nat64 = (prim "num_wrap_Int_Nat64" : Int -> Nat64) n;
func intToNat32Wrap(n : Int) : Nat32 = (prim "num_wrap_Int_Nat32" : Int -> Nat32) n;
func intToNat16Wrap(n : Int) : Nat16 = (prim "num_wrap_Int_Nat16" : Int -> Nat16) n;
func intToNat8Wrap(n : Int) : Nat8 = (prim "num_wrap_Int_Nat8" : Int -> Nat8) n;

func intToInt64Wrap(n : Int) : Int64 = (prim "num_wrap_Int_Int64" : Int -> Int64) n;
func intToInt32Wrap(n : Int) : Int32 = (prim "num_wrap_Int_Int32" : Int -> Int32) n;
func intToInt16Wrap(n : Int) : Int16 = (prim "num_wrap_Int_Int16" : Int -> Int16) n;
func intToInt8Wrap(n : Int) : Int8 = (prim "num_wrap_Int_Int8" : Int -> Int8) n;

func int64ToNat64(n : Int64) : Nat64 = (prim "num_wrap_Int64_Nat64" : Int64 -> Nat64) n;
func nat64ToInt64(n : Nat64) : Int64 = (prim "num_wrap_Nat64_Int64" : Nat64 -> Int64) n;
func int32ToNat32(n : Int32) : Nat32 = (prim "num_wrap_Int32_Nat32" : Int32 -> Nat32) n;
func nat32ToInt32(n : Nat32) : Int32 = (prim "num_wrap_Nat32_Int32" : Nat32 -> Int32) n;
func int16ToNat16(n : Int16) : Nat16 = (prim "num_wrap_Int16_Nat16" : Int16 -> Nat16) n;
func nat16ToInt16(n : Nat16) : Int16 = (prim "num_wrap_Nat16_Int16" : Nat16 -> Int16) n;
func int8ToNat8(n : Int8) : Nat8 = (prim "num_wrap_Int8_Nat8" : Int8 -> Nat8) n;
func nat8ToInt8(n : Nat8) : Int8 = (prim "num_wrap_Nat8_Int8" : Nat8 -> Int8) n;

// Char conversion and properties

func charToNat32(c : Char) : Nat32 = (prim "num_wrap_Char_Nat32" : Char -> Nat32) c;
func nat32ToChar(w : Nat32) : Char = (prim "num_conv_Nat32_Char" : Nat32 -> Char) w;

func charToText(c : Char) : Text = (prim "conv_Char_Text" : Char -> Text) c;

func charToUpper(c : Char) : Char = (prim "char_to_upper" : Char -> Char) c;
func charToLower(c : Char) : Char = (prim "char_to_lower" : Char -> Char) c;
func charIsWhitespace(c : Char) : Bool = (prim "char_is_whitespace" : Char -> Bool) c;
func charIsLowercase(c : Char) : Bool = (prim "char_is_lowercase" : Char -> Bool) c;
func charIsUppercase(c : Char) : Bool = (prim "char_is_uppercase" : Char -> Bool) c;
func charIsAlphabetic(c : Char) : Bool = (prim "char_is_alphabetic" : Char -> Bool) c;

// Exotic bitwise operations
func popcntNat8(w : Nat8) : Nat8 = (prim "popcnt8" : Nat8 -> Nat8) w;
func clzNat8(w : Nat8) : Nat8 = (prim "clz8" : Nat8 -> Nat8) w;
func ctzNat8(w : Nat8) : Nat8 = (prim "ctz8" : Nat8 -> Nat8) w;
func btstNat8(w : Nat8, amount : Nat8) : Bool = (prim "btst8" : (Nat8, Nat8) -> Nat8) (w, amount) != (0 : Nat8);

func popcntNat16(w : Nat16) : Nat16 = (prim "popcnt16" : Nat16 -> Nat16) w;
func clzNat16(w : Nat16) : Nat16 = (prim "clz16" : Nat16 -> Nat16) w;
func ctzNat16(w : Nat16) : Nat16 = (prim "ctz16" : Nat16 -> Nat16) w;
func btstNat16(w : Nat16, amount : Nat16) : Bool = (prim "btst16" : (Nat16, Nat16) -> Nat16) (w, amount) != (0 : Nat16);

func popcntNat32(w : Nat32) : Nat32 = (prim "popcnt32" : Nat32 -> Nat32) w;
func clzNat32(w : Nat32) : Nat32 = (prim "clz32" : Nat32 -> Nat32) w;
func ctzNat32(w : Nat32) : Nat32 = (prim "ctz32" : Nat32 -> Nat32) w;
func btstNat32(w : Nat32, amount : Nat32) : Bool = (prim "btst32" : (Nat32, Nat32) -> Nat32) (w, amount) != (0 : Nat32);

func popcntNat64(w : Nat64) : Nat64 = (prim "popcnt64" : Nat64 -> Nat64) w;
func clzNat64(w : Nat64) : Nat64 = (prim "clz64" : Nat64 -> Nat64) w;
func ctzNat64(w : Nat64) : Nat64 = (prim "ctz64" : Nat64 -> Nat64) w;
func btstNat64(w : Nat64, amount : Nat64) : Bool = (prim "btst64" : (Nat64, Nat64) -> Nat64) (w, amount) != (0 : Nat64);

func popcntInt8(w : Int8) : Int8 = (prim "popcnt8" : Int8 -> Int8) w;
func clzInt8(w : Int8) : Int8 = (prim "clz8" : Int8 -> Int8) w;
func ctzInt8(w : Int8) : Int8 = (prim "ctz8" : Int8 -> Int8) w;
func btstInt8(w : Int8, amount : Int8) : Bool = (prim "btst8" : (Int8, Int8) -> Int8) (w, amount) != (0 : Int8);

func popcntInt16(w : Int16) : Int16 = (prim "popcnt16" : Int16 -> Int16) w;
func clzInt16(w : Int16) : Int16 = (prim "clz16" : Int16 -> Int16) w;
func ctzInt16(w : Int16) : Int16 = (prim "ctz16" : Int16 -> Int16) w;
func btstInt16(w : Int16, amount : Int16) : Bool = (prim "btst16" : (Int16, Int16) -> Int16) (w, amount) != (0 : Int16);

func popcntInt32(w : Int32) : Int32 = (prim "popcnt32" : Int32 -> Int32) w;
func clzInt32(w : Int32) : Int32 = (prim "clz32" : Int32 -> Int32) w;
func ctzInt32(w : Int32) : Int32 = (prim "ctz32" : Int32 -> Int32) w;
func btstInt32(w : Int32, amount : Int32) : Bool = (prim "btst32" : (Int32, Int32) -> Int32) (w, amount) != (0 : Int32);

func popcntInt64(w : Int64) : Int64 = (prim "popcnt64" : Int64 -> Int64) w;
func clzInt64(w : Int64) : Int64 = (prim "clz64" : Int64 -> Int64) w;
func ctzInt64(w : Int64) : Int64 = (prim "ctz64" : Int64 -> Int64) w;
func btstInt64(w : Int64, amount : Int64) : Bool = (prim "btst64" : (Int64, Int64) -> Int64) (w, amount) != (0 : Int64);

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
