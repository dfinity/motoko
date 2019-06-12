let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Shared = prim "Shared";
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

(* Primitives *)

open Value

module Conv = struct
  open Nativeint
  open Big_int
  let of_signed_Word32 w = to_int (logand 0xFFFFFFFFn (of_int32 w))

  let twoRaised62 = power_int_positive_int 2 62
  let twoRaised63 = power_int_positive_int 2 63
  let word_twoRaised63 = Word64.(pow 2L 63L)
  let twoRaised64 = power_int_positive_int 2 64

  let word64_of_nat_big_int i =
    assert (sign_big_int i > -1);
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.add (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let word64_of_big_int i =
    let wrapped = mod_big_int i twoRaised64 in
    match int64_of_big_int_opt wrapped with
    | Some n -> n
    | _ -> Word64.sub (int64_of_big_int (sub_big_int wrapped twoRaised63)) word_twoRaised63

  let big_int_of_unsigned_word64 w =
    let i = big_int_of_int64 w in
    if sign_big_int i > -1 then i
    else add_big_int i twoRaised64

  let wrapped_int_of_big_int i =
    match int_of_big_int_opt i with
    | Some n -> n
    | _ -> int_of_big_int (mod_big_int i twoRaised62)

  (* for q in {0, -1} return i + q * offs *)
  let to_signed i q offs = if Big_int.sign_big_int q = 0 then i else i - offs
  let to_signed_big_int i q offs = Big_int.(if sign_big_int q = 0 then i else sub_big_int i offs)
end (* Conv *)


let prim = function
  | "abs" -> fun v k -> k (Int (Nat.abs (as_int v)))

  | "Nat8->Word8" -> fun v k ->
                     let i = Nat8.to_int (as_nat8 v)
                     in k (Word8 (Word8.of_int_u i))
  | "Nat->Word8" -> fun v k ->
                    let i = Conv.wrapped_int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_u i))
  | "Nat->Nat8" -> fun v k ->
                   let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 8) in
                   let i = Big_int.int_of_big_int r
                   in Big_int.(if eq_big_int q zero_big_int then k (Nat8 (Nat8.of_int i)) else assert false)
  | "Int8->Word8" -> fun v k ->
                     let i = Int_8.to_int (as_int8 v)
                     in k (Word8 (Word8.of_int_s i))
  | "Int->Word8" -> fun v k ->
                    let i = Conv.wrapped_int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_s i))
  | "Int->Int8" -> fun v k ->
                   let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 7) in
                   let i = Big_int.int_of_big_int r in
                   Big_int.
                    (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
                     then k (Int8(Int_8.of_int (Conv.to_signed i q 0x80))) else assert false)
  | "Nat16->Word16" -> fun v k ->
                       let i = Nat16.to_int (as_nat16 v)
                       in k (Word16 (Word16.of_int_u i))
  | "Nat->Word16" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_u i))
  | "Nat->Nat16" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 16) in
                    let i = Big_int.int_of_big_int r
                    in Big_int.(if eq_big_int q zero_big_int then k (Nat16 (Nat16.of_int i)) else assert false)
  | "Int16->Word16" -> fun v k ->
                       let i = Int_16.to_int (as_int16 v)
                       in k (Word16 (Word16.of_int_s i))
  | "Int->Word16" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_s i))
  | "Int->Int16" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 15) in
                    let i = Big_int.int_of_big_int r in
                    Big_int.
                       (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
                        then k (Int16(Int_16.of_int (Conv.to_signed i q 0x8000))) else assert false)
  | "Nat32->Word32" -> fun v k ->
                       let i = Nat32.to_int (as_nat32 v)
                       in k (Word32 (Word32.of_int_u i))
  | "Nat->Word32" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_u i))
  | "Nat->Nat32" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 32) in
                    let i = Big_int.int_of_big_int r
                    in Big_int.(if eq_big_int q zero_big_int then k (Nat32 (Nat32.of_int i)) else assert false)
  | "Int32->Word32" -> fun v k ->
                       let i = Int_32.to_int (as_int32 v)
                       in k (Word32 (Word32.of_int_s i))
  | "Int->Word32" -> fun v k ->
                     let i = Conv.wrapped_int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_s i))
  | "Int->Int32" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) (Big_int.power_int_positive_int 2 31) in
                    let i = Big_int.int_of_big_int r in
                    Big_int.
                       (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
                        then k (Int32 (Int_32.of_int (Conv.to_signed i q 0x80000000))) else assert false)

  | "Nat64->Word64" -> fun v k ->
                       let q, r = Big_int.quomod_big_int (Nat64.to_big_int (as_nat64 v)) Conv.twoRaised63 in
                       let i = Conv.to_signed_big_int r q Conv.twoRaised63 in
                       k (Word64 (Big_int.int64_of_big_int i))
  | "Nat->Word64" -> fun v k -> k (Word64 (Conv.word64_of_nat_big_int (as_int v)))
  | "Nat->Nat64" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised64 in
                    Big_int.
                      (if eq_big_int q zero_big_int
                       then k (Nat64 (Nat64.of_big_int r))
                       else assert false)
  | "Int64->Word64" -> fun v k -> k (Word64 (Big_int.int64_of_big_int (Int_64.to_big_int (as_int64 v))))
  | "Int->Word64" -> fun v k -> k (Word64 (Conv.word64_of_big_int (as_int v)))
  | "Int->Int64" -> fun v k ->
                    let q, r = Big_int.quomod_big_int (as_int v) Conv.twoRaised63 in
                    Big_int.
                      (if eq_big_int q zero_big_int || eq_big_int q (pred_big_int zero_big_int)
                       then k (Int64 (Int_64.of_big_int Conv.(to_signed_big_int r q twoRaised63)))
                       else assert false)

  | "Word8->Nat" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word8->Nat8" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
                     in k (Nat8 (Nat8.of_int i))
  | "Int8->Int" -> fun v k -> k (Int (Int.of_int (Int_8.to_int (as_int8 v))))
  | "Nat8->Nat" -> fun v k -> k (Int (Nat.of_int (Nat8.to_int (as_nat8 v))))
  | "Word8->Int" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word8->Int8" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
                    in k (Int8 (Int_8.of_int i))
  | "Word16->Nat" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word16->Nat16" -> fun v k ->
                       let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
                       in k (Nat16 (Nat16.of_int i))
  | "Int16->Int" -> fun v k -> k (Int (Int.of_int (Int_16.to_int (as_int16 v))))
  | "Nat16->Nat" -> fun v k -> k (Int (Nat.of_int (Nat16.to_int (as_nat16 v))))
  | "Word16->Int" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word16->Int16" -> fun v k ->
                       let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
                       in k (Int16 (Int_16.of_int i))
  | "Int32->Int" -> fun v k -> k (Int (Int.of_int (Int_32.to_int (as_int32 v))))
  | "Nat32->Nat" -> fun v k -> k (Int (Nat.of_int (Nat32.to_int (as_nat32 v))))
  | "Word32->Nat" -> fun v k ->
                     let i = Conv.of_signed_Word32 (as_word32 v)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word32->Int" -> fun v k -> k (Int (Big_int.big_int_of_int32 (as_word32 v)))
  | "Word32->Int32" -> fun v k ->
                       let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
                       k (Int32 (Int_32.of_int i))
  | "Word32->Nat32" -> fun v k ->
                       let i = Big_int.(int_of_big_int (big_int_of_int32 (as_word32 v))) in
                       let i' = if i < 0 then i + 0x100000000 else i in
                       k (Nat32 (Nat32.of_int i'))

  | "Int64->Int" -> fun v k -> k (Int (Int_64.to_big_int (as_int64 v)))
  | "Nat64->Nat" -> fun v k -> k (Int (Nat64.to_big_int (as_nat64 v)))
  | "Word64->Nat" -> fun v k ->
                     let i = Conv.big_int_of_unsigned_word64 (as_word64 v)
                     in k (Int i)
  | "Word64->Nat64" -> fun v k ->
                       let i = Conv.big_int_of_unsigned_word64 (as_word64 v)
                       in k (Nat64 (Nat64.of_big_int i))
  | "Word64->Int" -> fun v k -> k (Int (Big_int.big_int_of_int64 (as_word64 v)))
  | "Word64->Int64" -> fun v k ->
                       let i = Big_int.big_int_of_int64 (as_word64 v)
                       in k (Int64 (Int_64.of_big_int i))

  | "Char->Word32" -> fun v k ->
                      let i = as_char v
                      in k (Word32 (Word32.of_int_u i))
  | "Word32->Char" -> fun v k ->
                      let i = Conv.of_signed_Word32 (as_word32 v)
                      in k (Char i)

  | "popcnt8" | "popcnt16" | "popcnt" | "popcnt64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. popcnt w)
        | Word16 w -> Word16 (Word16.popcnt w)
        | Word32 w -> Word32 (Word32.popcnt w)
        | Word64 w -> Word64 (Word64.popcnt w)
        | _ -> failwith "popcnt")

  | "clz8" | "clz16" | "clz" | "clz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. clz w)
        | Word16 w -> Word16 (Word16.clz w)
        | Word32 w -> Word32 (Word32.clz w)
        | Word64 w -> Word64 (Word64.clz w)
        | _ -> failwith "clz")

  | "ctz8" | "ctz16" | "ctz" | "ctz64" ->
     fun v k ->
     k (match v with
        | Word8  w -> Word8  (Word8. ctz w)
        | Word16 w -> Word16 (Word16.ctz w)
        | Word32 w -> Word32 (Word32.ctz w)
        | Word64 w -> Word64 (Word64.ctz w)
        | _ -> failwith "ctz")

  | "btst8" | "btst16" | "btst" | "btst64" ->
     fun v k ->
     let w, a = as_pair v
     in k (match w with
           | Word8  y -> Word8  Word8. (and_ y (shl (of_int_u 1) (as_word8  a)))
           | Word16 y -> Word16 Word16.(and_ y (shl (of_int_u 1) (as_word16 a)))
           | Word32 y -> Word32 Word32.(and_ y (shl 1l (as_word32 a)))
           | Word64 y -> Word64 Word64.(and_ y (shl 1L (as_word64 a)))
           | _ -> failwith "btst")

  | "Char->Text" -> fun v k -> let str = match as_char v with
                                          | c when c <= 0o177 -> String.make 1 (Char.chr c)
                                          | code -> Wasm.Utf8.encode [code]
                               in k (Text str)
  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "rts_version" -> fun v k -> as_unit v; k (Text "0.1")
  | "idlHash" -> fun v k -> let s = as_text v in k (Word32 (IdlHash.idl_hash s))
  | "decodeUTF8" -> fun v k ->
                    let s = as_text v in
                    let open Int32 in
                    let take_and_mask bits offset =
                      logand (sub (shift_left 1l bits) 1l) (of_int (Char.code s.[offset])) in
                    let open List in
                    let classify_utf8_leader =
                      function
                      | ch when compare ch 0x80l < 0 -> map take_and_mask [7]
                      | ch when compare ch 0xe0l < 0 -> map take_and_mask [5; 6]
                      | ch when compare ch 0xf0l < 0 -> map take_and_mask [4; 6; 6]
                      | ch                           -> map take_and_mask [3; 6; 6; 6] in
                    let nobbles = mapi (fun i f -> f i) (classify_utf8_leader (of_int (Char.code s.[0]))) in
                    let code = fold_left (fun acc nobble -> logor (shift_left acc 6) nobble) 0l nobbles in
                    k (Tup [Word32 (of_int (length nobbles)); Char (to_int code)])
  | "@serialize" -> fun v k -> k (Serialized v)
  | "@deserialize" -> fun v k -> k (as_serialized v)
  | "Array.init" -> fun v k ->
    (match Value.as_tup v with
    | [len; x] ->
      k (Array (Array.init (Int.to_int (as_int len)) (fun _ -> Mut (ref x))))
    | _ -> assert false
    )
  | "Array.tabulate" -> fun v k ->
    (match Value.as_tup v with
    | [len; g] ->
      let len_nat = Int.to_int (as_int len) in
      let (_, g') = Value.as_func g in
      let rec go prefix k i =
        if i == len_nat
        then k (Array (Array.of_list (prefix [])))
        else g' (Int (Int.of_int i)) (fun x -> go (fun tl -> prefix (x::tl)) k (i + 1))
      in go (fun xs -> xs) k 0
    | _ -> assert false
    )
  | s -> raise (Invalid_argument ("Value.prim: " ^ s))
