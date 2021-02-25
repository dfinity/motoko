/*
The primitive definitions.

This module should contain everything that cannot be implemented in plain
Motoko. It is available via `import Prim "mo:prim"`. Normal user code would
usually not import that module directly, but through `base`, which takes
care of providing a proper module structure, e.g. exposing `Array_tabulate`
through `Array.tabulate`.

Therefore, the prim module does not need to provide a stable interface, as its
only supported consumer is the `base` library, and that is bundled with the
compiler.

Nevertheless, it shoud be _safe_ to import prim, i.e. the definitions here
should not break type safety or other guarantees of the language.
*/

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

// Total conversions (fixed to big)

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

// Trapping conversions (big to fixed)

func intToInt64(n : Int) : Int64 = (prim "num_conv_Int_Int64" : Int -> Int64) n;
func intToInt32(n : Int) : Int32 = (prim "num_conv_Int_Int32" : Int -> Int32) n;
func intToInt16(n : Int) : Int16 = (prim "num_conv_Int_Int16" : Int -> Int16) n;
func intToInt8(n : Int) : Int8 = (prim "num_conv_Int_Int8" : Int -> Int8) n;

func natToNat64(n : Nat) : Nat64 = (prim "num_conv_Nat_Nat64" : Nat -> Nat64) n;
func natToNat32(n : Nat) : Nat32 = (prim "num_conv_Nat_Nat32" : Nat -> Nat32) n;
func natToNat16(n : Nat) : Nat16 = (prim "num_conv_Nat_Nat16" : Nat -> Nat16) n;
func natToNat8(n : Nat) : Nat8 = (prim "num_conv_Nat_Nat8" : Nat -> Nat8) n;

// Wrapping conversions (big to fixed, big to word, and within fixed)

func intToInt64Wrap(n : Int) : Int64 = (prim "num_wrap_Int_Int64" : Int -> Int64) n;
func intToInt32Wrap(n : Int) : Int32 = (prim "num_wrap_Int_Int32" : Int -> Int32) n;
func intToInt16Wrap(n : Int) : Int16 = (prim "num_wrap_Int_Int16" : Int -> Int16) n;
func intToInt8Wrap(n : Int) : Int8 = (prim "num_wrap_Int_Int8" : Int -> Int8) n;

func intToNat64Wrap(n : Int) : Nat64 = (prim "num_wrap_Int_Nat64" : Int -> Nat64) n;
func intToNat32Wrap(n : Int) : Nat32 = (prim "num_wrap_Int_Nat32" : Int -> Nat32) n;
func intToNat16Wrap(n : Int) : Nat16 = (prim "num_wrap_Int_Nat16" : Int -> Nat16) n;
func intToNat8Wrap(n : Int) : Nat8 = (prim "num_wrap_Int_Nat8" : Int -> Nat8) n;

func intToWord64Wrap(n : Int) : Word64 = (prim "num_wrap_Int_Word64" : Int -> Word64) n;
func intToWord32Wrap(n : Int) : Word32 = (prim "num_wrap_Int_Word32" : Int -> Word32) n;
func intToWord16Wrap(n : Int) : Word16 = (prim "num_wrap_Int_Word16" : Int -> Word16) n;
func intToWord8Wrap(n : Int) : Word8 = (prim "num_wrap_Int_Word8" : Int -> Word8) n;

func int64ToWord64(n : Int64) : Word64 = (prim "num_wrap_Int64_Word64" : Int64 -> Word64) n;
func word64ToInt64(n : Word64) : Int64 = (prim "num_wrap_Word64_Int64" : Word64 -> Int64) n;
func int32ToWord32(n : Int32) : Word32 = (prim "num_wrap_Int32_Word32" : Int32 -> Word32) n;
func word32ToInt32(n : Word32) : Int32 = (prim "num_wrap_Word32_Int32" : Word32 -> Int32) n;
func int16ToWord16(n : Int16) : Word16 = (prim "num_wrap_Int16_Word16" : Int16 -> Word16) n;
func word16ToInt16(n : Word16) : Int16 = (prim "num_wrap_Word16_Int16" : Word16 -> Int16) n;
func int8ToWord8(n : Int8) : Word8 = (prim "num_wrap_Int8_Word8" : Int8 -> Word8) n;
func word8ToInt8(n : Word8) : Int8 = (prim "num_wrap_Word8_Int8" : Word8 -> Int8) n;

func word64ToNat64(n : Word64) : Nat64 = (prim "num_wrap_Word64_Nat64" : Word64 -> Nat64) n;
func nat64ToWord64(n : Nat64) : Word64 = (prim "num_wrap_Nat64_Word64" : Nat64 -> Word64) n;
func word32ToNat32(n : Word32) : Nat32 = (prim "num_wrap_Word32_Nat32" : Word32 -> Nat32) n;
func nat32ToWord32(n : Nat32) : Word32 = (prim "num_wrap_Nat32_Word32" : Nat32 -> Word32) n;
func word16ToNat16(n : Word16) : Nat16 = (prim "num_wrap_Word16_Nat16" : Word16 -> Nat16) n;
func nat16ToWord16(n : Nat16) : Word16 = (prim "num_wrap_Nat16_Word16" : Nat16 -> Word16) n;
func word8ToNat8(n : Word8) : Nat8 = (prim "num_wrap_Word8_Nat8" : Word8 -> Nat8) n;
func nat8ToWord8(n : Nat8) : Word8 = (prim "num_wrap_Nat8_Word8" : Nat8 -> Word8) n;

// Temporary, to break less of base in #2324
let natToWord8 : Nat -> Word8 = intToWord8Wrap;
let intToWord8 : Int -> Word8 = intToWord8Wrap;
let natToWord16 : Nat -> Word16 = intToWord16Wrap;
let intToWord16 : Int -> Word16 = intToWord16Wrap;
let natToWord32 : Nat -> Word32 = intToWord32Wrap;
let intToWord32 : Int -> Word32 = intToWord32Wrap;
let natToWord64 : Nat -> Word64 = intToWord64Wrap;
let intToWord64 : Int -> Word64 = intToWord64Wrap;

func int64ToNat64(n : Int64) : Nat64 = (prim "num_wrap_Int64_Nat64" : Int64 -> Nat64) n;
func nat64ToInt64(n : Nat64) : Int64 = (prim "num_wrap_Nat64_Int64" : Nat64 -> Int64) n;
func int32ToNat32(n : Int32) : Nat32 = (prim "num_wrap_Int32_Nat32" : Int32 -> Nat32) n;
func nat32ToInt32(n : Nat32) : Int32 = (prim "num_wrap_Nat32_Int32" : Nat32 -> Int32) n;
func int16ToNat16(n : Int16) : Nat16 = (prim "num_wrap_Int16_Nat16" : Int16 -> Nat16) n;
func nat16ToInt16(n : Nat16) : Int16 = (prim "num_wrap_Nat16_Int16" : Nat16 -> Int16) n;
func int8ToNat8(n : Int8) : Nat8 = (prim "num_wrap_Int8_Nat8" : Int8 -> Nat8) n;
func nat8ToInt8(n : Nat8) : Int8 = (prim "num_wrap_Nat8_Int8" : Nat8 -> Int8) n;

// Char conversion and properties

func charToWord32(c : Char) : Word32 = (prim "num_wrap_Char_Word32" : Char -> Word32) c;
func word32ToChar(w : Word32) : Char = (prim "num_conv_Word32_Char" : Word32 -> Char) w;
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

