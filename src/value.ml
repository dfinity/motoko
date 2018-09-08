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
  include module type of Z
  val sub : t -> t -> t
  val pow' : t -> t -> t
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
end

module Int : NumType with type t = Z.t =
struct
  include Z
  let of_string s = Z.of_string (String.concat "" (String.split_on_char '_' s))
  let eq = equal
  let ne x y = not (eq x y)
  let le = leq
  let ge = geq

  let max_int = of_int max_int

  let pow' x y =
    if gt y max_int
    then raise (Invalid_argument "Int.pow")
    else pow x (to_int y)
end

module Nat : NumType with type t = Z.t =
struct
  include Int
  let sub x y =
    let z = Int.sub x y in
    if ge z zero then z else raise (Invalid_argument "Nat.sub")
end


(* Types *)

type unicode = int

type value =
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
  | Obj of value Env.t
  | Array of value array
  | Func of (value -> value cont -> unit)
  | Async of async
  | Mut of value ref

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit

let unit = Tup []

let invalid s = raise (Invalid_argument s)

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
let as_obj = function Obj ve -> ve | _ -> invalid "as_obj"
let as_func = function Func f -> f | _ -> invalid "as_func"
let as_async = function Async a -> a | _ -> invalid "as_async"
let as_mut = function Mut r -> r | _ -> invalid "as_mut"


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
  | Obj ve ->
    if d = 0 then "{...}" else
    sprintf "{%s}" (String.concat "; " (List.map (fun (x, v) ->
      sprintf "%s = %s" x (string_of_val' (d - 1) v)) (Env.bindings ve)))
  | Array a ->
    sprintf "[%s]" (String.concat ", "
      (List.map (string_of_val' d) (Array.to_list a)))
  | Func f -> "func"
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
