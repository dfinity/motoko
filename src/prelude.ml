let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Class = prim "Class";
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

type Iter<T_> = {next : () -> T_?};

func abs (x : Int) : Nat { let p : Int -> Nat = prim "abs"; p x };

func ignore (_ : Any) {};

class range(x : Nat, y : Nat) {
  private var i = x;
  next() : Nat? { if (i > y) null else {let j = i; i += 1; j?} };
};

class revrange(x : Nat, y : Nat) {
  private var i = x + 1;
  next() : Nat? { if (i <= y) null else {i -= 1; i?} };
};

func printInt (x : Int) { let p : Int -> () = prim "printInt"; p x; };
func print (x : Text) { let p : Text -> () = prim "print"; p x; };
|}


(* Primitives *)

open Value

let prim = function
  | "abs" -> fun v k -> k (Nat (Nat.abs (as_int v)))
  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "printInt" ->
    fun v k ->
      Printf.printf "printInt(%s)\n%!" (Int.to_string (as_int v));
      k unit
  | _ -> raise (Invalid_argument "Value.prim")
