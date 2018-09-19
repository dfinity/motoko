open Printf


(* Environments *)

module Env = Env.Make(String) 


(* Numeric Representations *)

(* Represent n-bit integers using k-bit (n<=k) integers by shifting left/right by k-n bits *)
module SubRep(Rep : Wasm.Int.RepType)(Width : sig val bitwidth : int end) :
  Wasm.Int.RepType with type t = Rep.t =
struct
  let _ = assert (Width.bitwidth < Rep.bitwidth)

  type t = Rep.t

  let bitwidth = Width.bitwidth
  let bitdiff = Rep.bitwidth - Width.bitwidth
  let inj r  = Rep.shift_left r bitdiff
  let proj i = Rep.shift_right i bitdiff

  let zero = inj Rep.zero
  let one = inj Rep.one
  let minus_one = inj Rep.minus_one
  let max_int = inj (Rep.shift_right_logical Rep.max_int bitdiff)
  let min_int = inj (Rep.shift_right_logical Rep.min_int bitdiff)
  let neg i = inj (Rep.neg (proj i))
  let add i j = inj (Rep.add (proj i) (proj j))
  let sub i j = inj (Rep.sub (proj i) (proj j))
  let mul i j = inj (Rep.mul (proj i) (proj j)) 
  let div i j = inj (Rep.div (proj i) (proj j))
  let rem i j = inj (Rep.rem (proj i) (proj j))
  let logand = Rep.logand
  let logor = Rep.logor
  let lognot i = inj (Rep.lognot (proj i))
  let logxor i j = inj (Rep.logxor (proj i) (proj j))
  let shift_left i j = Rep.shift_left i j
  let shift_right = Rep.shift_right
  let shift_right_logical = Rep.shift_right_logical
  let of_int i = inj (Rep.of_int i)
  let to_int i = Rep.to_int (proj i)
  let to_string i = Rep.to_string (proj i)
end

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
  val pow : t -> t -> t
end

module MakeWord(WasmInt : Wasm.Int.S) =
struct
  include WasmInt
  let neg w = sub zero w
  let not w = xor w (of_int_s (-1))
  let one = of_int_u 1
  let rec pow x y =
    if y = zero then
      one
    else if and_ y one = zero then
      pow (mul x x) (shr_u y one)
    else
      mul x (pow x (sub y one))
end

module Int32Rep = struct include Int32 let bitwidth = 32 end
module Int16Rep = SubRep(Int32Rep)(struct let bitwidth = 16 end)
module Int8Rep = SubRep(Int32Rep)(struct let bitwidth = 8 end)

module Word8 = MakeWord(Wasm.Int.Make(Int8Rep))
module Word16 = MakeWord(Wasm.Int.Make(Int16Rep))
module Word32 = MakeWord(Wasm.I32)
module Word64 = MakeWord(Wasm.I64)

module type FloatType =
sig
  include Wasm.Float.S
  val pow : t -> t -> t
end

module MakeFloat(WasmFloat : Wasm.Float.S) =
struct
  include WasmFloat
  let pow x y = of_float (to_float x ** to_float y)
end

module Float = MakeFloat(Wasm.F64)


module type NumType =
sig
  type t
  val zero : t
  val abs : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val pow : t -> t -> t
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val compare : t -> t -> int
  val to_int : t -> int
  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string
end

module Int : NumType with type t = Big_int.big_int =
struct
  open Big_int
  type t = big_int
  let zero = zero_big_int
  let sub = sub_big_int
  let abs = abs_big_int
  let neg = minus_big_int
  let add = add_big_int
  let mul = mult_big_int
  let div = div_big_int
  let rem = mod_big_int (* Is rem and mod the same here? *)
  let eq = eq_big_int
  let ne x y = not (eq x y)
  let lt = lt_big_int
  let gt = gt_big_int
  let le = le_big_int
  let ge = ge_big_int
  let compare = compare_big_int
  let to_int = int_of_big_int
  let of_int = big_int_of_int
  let to_string = string_of_big_int
  let of_string s =
    big_int_of_string (String.concat "" (String.split_on_char '_' s))

  let max_int = big_int_of_int max_int

  let pow x y =
    if gt y max_int
    then raise (Invalid_argument "Int.pow")
    else power_big_int_positive_int x (int_of_big_int y)
end

module Nat : NumType with type t = Big_int.big_int =
struct
  include Int
  let sub x y =
    let z = Int.sub x y in
    if ge z zero then z else raise (Invalid_argument "Nat.sub")
end


(* Types *)

type unicode = int
type class_ = int

type func = value -> value cont -> unit
and value =
  | Null
  | Bool of bool
  | Nat of Nat.t
  | Int of Int.t
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Tup of value list
  | Array of value array
  | Obj of class_ option * value Env.t
  | Func of class_ option * func
  | Async of async
  | Mut of value ref

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Classes *)

let class_counter = ref 0

let new_class () = incr class_counter; !class_counter


(* Projections *)

let invalid s = raise (Invalid_argument ("Value." ^ s))

let as_null = function Null -> () | _ -> invalid "as_null"
let as_bool = function Bool b -> b | _ -> invalid "as_bool"
let as_nat = function Nat n -> n | _ -> invalid "as_nat"
let as_int = function Int n -> n | Nat n -> n | _ -> invalid "as_int"
let as_word8 = function Word8 w -> w | _ -> invalid "as_word8"
let as_word16 = function Word16 w -> w | _ -> invalid "as_word16"
let as_word32 = function Word32 w -> w | _ -> invalid "as_word32"
let as_word64 = function Word64 w -> w | _ -> invalid "as_word64"
let as_float = function Float f -> f | _ -> invalid "as_float"
let as_char = function Char c -> c | _ -> invalid "as_char"
let as_text = function Text s -> s | _ -> invalid "as_text"
let as_array = function Array a -> a | _ -> invalid "as_array"
let as_tup = function Tup vs -> vs | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [v1; v2] -> v1, v2 | _ -> invalid "as_pair"
let as_string = function Text t -> t | _ -> invalid "as_text"

let obj_of_array a =
  let get =
    Func (None, fun v k ->
      let n = as_nat v in
      if Nat.lt n (Nat.of_int (Array.length a)) then
        k (a.(Nat.to_int n))
      else
        raise (Invalid_argument "array index out of bounds")
    )
  in
  let set =
    Func (None, fun v k ->
      let v1, v2 = as_pair v in
      let n = as_nat v1 in
      if Nat.lt n (Nat.of_int (Array.length a)) then
        k (a.(Nat.to_int n) <- v2; Tup [])
      else
        raise (Invalid_argument "array index out of bounds")
    )
  in
  let len =
    Func (None, fun v k -> as_unit v; k (Nat (Nat.of_int (Array.length a))))
  in
  let keys =
    Func (None, fun v k ->
      as_unit v;
      let i = ref 0 in
      let next = fun v k' ->
        if !i = Array.length a then k' Null else
        let v = Nat (Nat.of_int !i) in incr i; k' v
      in k (Obj (None, Env.singleton "next" (Func (None, next))))
    )
  in
  let vals =
    Func (None, fun v k ->
      as_unit v;
      let i = ref 0 in
      let next = fun v k' ->
        if !i = Array.length a then k' Null else
        let v = a.(!i) in incr i; k' v
      in k (Obj (None, Env.singleton "next" (Func (None, next))))
    )
  in
  Env.from_list ["get", get; "set", set; "len", len; "keys", keys; "vals", vals]

let as_obj = function Obj (co, ve) -> co, ve | Array a -> None, obj_of_array a | _ -> invalid "as_obj"
let as_func = function Func (co, f) -> co, f | _ -> invalid "as_func"
let as_async = function Async a -> a | _ -> invalid "as_async"
let as_mut = function Mut r -> r | _ -> invalid "as_mut"


(* Primitives *)

let unit = Tup []

let prim = function
  | "abs" -> fun v k -> k (Nat (Nat.abs (as_int v)))
  | "print" -> fun v k -> Printf.printf "%s" (as_string v); k unit
  | "printInt" ->
    fun v k ->
      Printf.printf "printInt(%s)\n" (Int.to_string (as_int v));
      k unit
  | _ -> raise (Invalid_argument "Value.prim")


(* Ordering *)

let generic_compare = compare

let rec compare x1 x2 =
  if x1 == x2 then 0 else
  match x1, x2 with
  | Nat n1, Nat n2 -> Nat.compare n1 n2
  | Int n1, Int n2 -> Int.compare n1 n2
  | Tup vs1, Tup vs2 -> Lib.List.compare compare vs1 vs2
  | Array a1, Array a2 -> Lib.Array.compare compare a1 a2
  | Obj (c1, fs1), Obj (c2, fs2) ->
    (match generic_compare c1 c2 with
    | 0 -> Env.compare compare fs1 fs2
    | n -> n
    )
  | Mut r1, Mut r2 -> compare !r1 !r2
  | Async _, Async _ -> raise (Invalid_argument "Value.compare")
  | _ -> generic_compare x1 x2


(* Pretty Printing *)

let add_unicode buf = function
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0a -> Buffer.add_string buf "\\n"
  | 0x22 -> Buffer.add_string buf "\\\""
  | 0x27 -> Buffer.add_string buf "\\\'"
  | 0x5c -> Buffer.add_string buf "\\\\"
  | c when 0x20 <= c && c < 0x7f -> Buffer.add_char buf (Char.chr c)
  | c -> Printf.bprintf buf "\\u{%02x}" c

let string_of_string lsep s rsep =
  let buf = Buffer.create 256 in
  Buffer.add_char buf lsep;
  List.iter (add_unicode buf) s;
  Buffer.add_char buf rsep;
  Buffer.contents buf

let rec string_of_val_nullary d = function
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Nat n -> Nat.to_string n
  | Int i -> Int.to_string i
  | Word8 w -> Word8.to_string_u w
  | Word16 w -> Word16.to_string_u w
  | Word32 w -> Word32.to_string_u w
  | Word64 w -> Word64.to_string_u w
  | Float f -> Float.to_string f
  | Char c -> string_of_string '\'' [c] '\''
  | Text t -> string_of_string '\"' (Wasm.Utf8.decode t) '\"'
  | Tup vs ->
    sprintf "(%s%s)"
      (String.concat ", " (List.map (string_of_val' d) vs))
      (if List.length vs = 1 then "," else "")
  | Obj (_, ve) ->
    if d = 0 then "{...}" else
    sprintf "{%s}" (String.concat "; " (List.map (fun (x, v) ->
      sprintf "%s = %s" x (string_of_val' (d - 1) v)) (Env.bindings ve)))
  | Array a ->
    sprintf "[%s]" (String.concat ", "
      (List.map (string_of_val' d) (Array.to_list a)))
  | Func (None, _) -> "func"
  | Func (Some _, _) -> "class"
  | v -> "(" ^ string_of_val' d v ^ ")"

and string_of_val' d = function
  | Async {result; waiters = []} ->
    sprintf "async %s" (string_of_def_nullary d result)
  | Async {result; waiters} ->
    sprintf "async[%d] %s"
      (List.length waiters) (string_of_def_nullary d result)
  | Mut r -> sprintf "%s" (string_of_val' d !r)
  | v -> string_of_val_nullary d v

and string_of_def_nullary d def =
  match Lib.Promise.value_opt def with
  | Some v -> string_of_val_nullary d v
  | None -> "_"

and string_of_def' d def =
  match Lib.Promise.value_opt def with
  | Some v -> string_of_val' d v
  | None -> "_"

let string_of_val v = string_of_val' !Flags.print_depth v
let string_of_def d = string_of_def' !Flags.print_depth d
