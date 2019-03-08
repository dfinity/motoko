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

func printInt(x : Int) { (prim "printInt" : Int -> ()) x };
func print(x : Text) { (prim "print" : Text -> ()) x };

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

func charToWord32(c : Char) : Word32 = (prim "Char->Word32" : Char -> Word32) c;
func word32ToChar(w : Word32) : Char = (prim "Word32->Char" : Word32 -> Char) w;


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
  func fullfill(t:T):() {
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
  (enqueue,fullfill)
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

  | "Char->Word32" -> fun v k ->
                      let i = as_char v
                      in k (Word32 (Word32.of_int_u i))
  | "Word32->Char" -> fun v k ->
                      let i = Conv.of_signed_Word32 (as_word32 v)
                      in k (Char i)

  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "printInt" -> fun v k -> Printf.printf "%d%!" (Int.to_int (as_int v)); k unit
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
