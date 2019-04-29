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

func charToText(c : Char) : Text = (prim "Char->Text" : Char -> Text) c;

func printInt(x : Int) { (prim "printInt" : Int -> ()) x };
func printChar(x : Char) { print (charToText x) };
func print(x : Text) { (prim "print" : Text -> ()) x };

// Hashing
func hashInt(n : Int) : Word32 = (prim "Int~hash" : Int -> Word32) n;

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
func shrsWord8(w : Word8, amount : Word8) : Word8 = (prim "shrs8" : (Word8, Word8) -> Word8) (w, amount);
func popcntWord8(w : Word8) : Word8 = (prim "popcnt8" : Word8 -> Word8) w;
func clzWord8(w : Word8) : Word8 = (prim "clz8" : Word8 -> Word8) w;
func ctzWord8(w : Word8) : Word8 = (prim "ctz8" : Word8 -> Word8) w;
func btstWord8(w : Word8, amount : Word8) : Bool = (prim "btst8" : (Word8, Word8) -> Word8) (w, amount) != (0 : Word8);

func shrsWord16(w : Word16, amount : Word16) : Word16 = (prim "shrs16" : (Word16, Word16) -> Word16) (w, amount);
func popcntWord16(w : Word16) : Word16 = (prim "popcnt16" : Word16 -> Word16) w;
func clzWord16(w : Word16) : Word16 = (prim "clz16" : Word16 -> Word16) w;
func ctzWord16(w : Word16) : Word16 = (prim "ctz16" : Word16 -> Word16) w;
func btstWord16(w : Word16, amount : Word16) : Bool = (prim "btst16" : (Word16, Word16) -> Word16) (w, amount) != (0 : Word16);

func shrsWord32(w : Word32, amount : Word32) : Word32 = (prim "shrs" : (Word32, Word32) -> Word32) (w, amount);
func popcntWord32(w : Word32) : Word32 = (prim "popcnt" : Word32 -> Word32) w;
func clzWord32(w : Word32) : Word32 = (prim "clz" : Word32 -> Word32) w;
func ctzWord32(w : Word32) : Word32 = (prim "ctz" : Word32 -> Word32) w;
func btstWord32(w : Word32, amount : Word32) : Bool = (prim "btst" : (Word32, Word32) -> Word32) (w, amount) != (0 : Word32);

func shrsWord64(w : Word64, amount : Word64) : Word64 = (prim "shrs64" : (Word64, Word64) -> Word64) (w, amount);
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
  let of_signed_Word32 w = to_int (logand 0xFFFFFFFFn (of_int32 w))
end (* Conv *)


let prim = function
  | "abs" -> fun v k -> k (Int (Nat.abs (as_int v)))

  | "Int~hash" -> fun v k ->
                  let i = Word64.of_int_s (Big_int.int_of_big_int (as_int v)) in
                  let j = Word64.(and_ 0xFFFFFFFFL (xor (shr_u i 32L) i))
                  in k (Word32 (Word32.of_int_u (Int64.to_int j)))
  | "Nat->Word8" -> fun v k ->
                    let i = Big_int.int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_u i))
  | "Int->Word8" -> fun v k ->
                    let i = Big_int.int_of_big_int (as_int v)
                    in k (Word8 (Word8.of_int_s i))
  | "Nat->Word16" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_u i))
  | "Int->Word16" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word16 (Word16.of_int_s i))
  | "Nat->Word32" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_u i))
  | "Int->Word32" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word32 (Word32.of_int_s i))

  | "Nat->Word64" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word64 (Word64.of_int_u i))
  | "Int->Word64" -> fun v k ->
                     let i = Big_int.int_of_big_int (as_int v)
                     in k (Word64 (Word64.of_int_s i))

  | "Word8->Nat" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right_logical (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word8->Int" -> fun v k ->
                    let i = Int32.to_int (Int32.shift_right (Word8.to_bits (as_word8 v)) 24)
                    in k (Int (Big_int.big_int_of_int i))
  | "Word16->Nat" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right_logical (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word16->Int" -> fun v k ->
                     let i = Int32.to_int (Int32.shift_right (Word16.to_bits (as_word16 v)) 16)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word32->Nat" -> fun v k ->
                     let i = Conv.of_signed_Word32 (as_word32 v)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word32->Int" -> fun v k -> k (Int (Big_int.big_int_of_int32 (as_word32 v)))

  | "Word64->Nat" -> fun v k ->
                     let i = Int64.to_int (as_word64 v) (* ! *)
                     in k (Int (Big_int.big_int_of_int i))
  | "Word64->Int" -> fun v k -> k (Int (Big_int.big_int_of_int64 (as_word64 v)))

  | "Char->Word32" -> fun v k ->
                      let i = as_char v
                      in k (Word32 (Word32.of_int_u i))
  | "Word32->Char" -> fun v k ->
                      let i = Conv.of_signed_Word32 (as_word32 v)
                      in k (Char i)

  | "shrs8" | "shrs16" | "shrs" | "shrs64" ->
     fun v k ->
     let w, a = as_pair v
     in k (match w with
           | Word8  y -> Word8  (Word8 .shr_s y  (as_word8  a))
           | Word16 y -> Word16 (Word16.shr_s y  (as_word16 a))
           | Word32 y -> Word32 (Word32.shr_s y  (as_word32 a))
           | Word64 y -> Word64 (Word64.shr_s y  (as_word64 a))
           | _ -> failwith "shrs")

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
  | "printInt" -> fun v k -> Printf.printf "%d%!" (Int.to_int (as_int v)); k unit
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
