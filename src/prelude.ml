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

// Internal helper functions for the show translation

// The @ in the name ensures that this connot be shadowed by user code, so
// compiler passes can rely on them being in scope
// The text_of functions do not need to be exposed; the user can just use
// the show above.

func @text_of_Nat(x : Nat) : Text {
  var text = "";
  var n = x;
  let base = 10;

  while (n > 0) {
    let rem = n % base;
    text := (switch (rem) {
      case (0) { "0" };
      case (1) { "1" };
      case (2) { "2" };
      case (3) { "3" };
      case (4) { "4" };
      case (5) { "5" };
      case (6) { "6" };
      case (7) { "7" };
      case (8) { "8" };
      case (9) { "9" };
      case (_) { assert false; "" };
    }) # text;
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

func @text_option<T>(f : T -> Text, x : ?T) : Text {
  switch (x) {
    case (?y) {"?(" # f y # ")"};
    case null {"null"};
  }
};

func @text_array<T>(f : T -> Text, xs : [T]) : Text {
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

func @text_array_mut<T>(f : T -> Text, xs : [var T]) : Text {
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

let prim = function
  | "abs" -> fun v k -> k (Int (Nat.abs (as_int v)))
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
