let prelude =
{|
type Any = prim "Any";
type None = prim "None";
type Shared = prim "Shared";
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

func abs (x : Int) : Nat { ((prim "abs") : Int -> Nat) x };

func ignore (_ : Any) {};

class range(x : Nat, y : Nat) {
  private var i = x;
  next() : Nat? { if (i > y) null else {let j = i; i += 1; j?} };
};

class revrange(x : Nat, y : Nat) {
  private var i = x + 1;
  next() : Nat? { if (i <= y) null else {i -= 1; i?} };
};

func printInt (x : Int) { ((prim "printInt") : Int -> ()) x };
func print (x : Text) { ((prim "print") : Text -> ()) x };

/* This would be nicer as a objects, but lets do them as functions
   until the compiler has a concept of “static objects” */
func Array_init <T> (len : Nat,  x : T) : var T[] {
  ((prim "Array.init") : <T> (Nat, T) -> var T[]) <T>(len, x)
};
func Array_tabular <T> (len : Nat,  gen : Nat -> T) : T[] {
  ((prim "Array.tabular") : <T> (Nat, Nat -> T) -> T[]) <T>(len, gen)
};

|}

(*
type cont<T> = T -> () ;
type cps<T> = cont<T> -> ();

func new_async<T>(e:cps<T>) : async T  = {
  let async_ = ((prim "@make_async") : () -> async T)();
  func k(t:T):() =  ((prim "@set_async" ) : (async T,T)->() ) (async_,t);
  ((prim "@scheduler_queue") : cont<()> -> () ) (func () : () = e k);
  async_
};
*)

(* Primitives *)

open Value

let prim = function
  | "abs" -> fun v k -> k (Nat (Nat.abs (as_int v)))
  | "print" -> fun v k -> Printf.printf "%s%!" (as_text v); k unit
  | "printInt" ->
    fun v k ->
      Printf.printf "printInt(%s)\n%!" (Int.to_string (as_int v));
      k unit
  | "Array.init" -> fun v k ->
      (match Value.as_tup v with
       | [len; x] ->
         k (Array (Array.make (Int.to_int (as_nat len)) x))
      | _ -> assert false)
  | "Array.tabular" -> fun v k ->
      (match Value.as_tup v with
       | [len; g] ->
         let len_nat = Int.to_int (as_nat len) in
         let (_, g') = Value.as_func g in
	 let rec go prefix k i =
          if i == len_nat
          then k (Array (Array.of_list (prefix [])))
          else g' (Nat (Int.of_int i)) (fun x -> go (fun tl -> prefix (x::tl)) k (i + 1))
	 in go (fun xs -> xs) k 0
      | _ -> assert false)
  | s -> raise (Invalid_argument ("Value.prim: " ^ s))
