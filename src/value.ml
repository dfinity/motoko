open Printf
(*TBR*)

module T = Type


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
end

module MakeWord(WasmInt : Wasm.Int.S) =
struct
  include WasmInt
  let neg w = sub zero w
  let not w = xor w (of_int_s (-1))
end

module Int32Rep = struct include Int32 let bitwidth = 32 end
module Int16Rep = SubRep(Int32Rep)(struct let bitwidth = 16 end)
module Int8Rep = SubRep(Int32Rep)(struct let bitwidth = 8 end)

module Word8 = MakeWord(Wasm.Int.Make(Int8Rep))
module Word16 = MakeWord(Wasm.Int.Make(Int16Rep))
module Word32 = MakeWord(Wasm.I32)
module Word64 = MakeWord(Wasm.I64)
module Float = Wasm.F64

module type NumType =
sig
  include module type of Z
  val sub : t -> t -> t
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

let string_of_char c =
  let buf = Buffer.create 10 in
  Buffer.add_char buf '\"';
  add_unicode buf c;
  Buffer.add_char buf '\"';
  Buffer.contents buf

let string_of_text s =
  let buf = Buffer.create (String.length s + 256) in
  Buffer.add_char buf '\"';
  List.iter (add_unicode buf) (Wasm.Utf8.decode s);
  Buffer.add_char buf '\"';
  Buffer.contents buf

let rec string_of_val_nullary d conenv t v =
  match T.normalize conenv t with
  | T.Any -> "any"
  | T.Prim T.Null -> as_null v; "null"
  | T.Prim T.Bool -> if as_bool v then "true" else "false"
  | T.Prim T.Nat -> Nat.to_string (as_nat v)
  | T.Prim T.Int ->  Int.to_string (as_int v)
  | T.Prim T.Word8 -> Word8.to_string_u (as_word8 v)
  | T.Prim T.Word16 -> Word16.to_string_u (as_word16 v)
  | T.Prim T.Word32 -> Word32.to_string_u (as_word32 v)
  | T.Prim T.Word64 -> Word64.to_string_u (as_word64 v)
  | T.Prim T.Float -> Float.to_string (as_float v)
  | T.Prim T.Char -> string_of_char (as_char v)
  | T.Prim T.Text -> string_of_text (as_text v)
  | T.Var (c, []) -> Con.to_string c
  | T.Var (c, ts) ->
    sprintf "%s<%s>"
      (Con.to_string c)
      (String.concat ", " (List.map T.string_of_typ ts))
  | T.Tup ts ->
    let vs = as_tup v in
    sprintf "(%s)"
      (String.concat ", " (List.map2 (string_of_val' d conenv) ts vs))
  | T.Array t ->
    let a = as_array v in
    sprintf "[%s]" (String.concat ", "
      (List.map (string_of_val' d conenv t) (Array.to_list a)))
  | T.Obj (T.Object, fs) ->
    let ve = as_obj v in
    sprintf "{%s}"
      (if d = 0 then "..." else
       String.concat "; " (List.map (fun {T.name; typ} ->
        sprintf "%s = %s" name (string_of_val' (d - 1) conenv typ (Env.find name ve))
      ) fs))
  | T.Func _ ->
    "func"
  | T.Mut t ->
    string_of_val_nullary d conenv t !(as_mut v)
  | _ ->
    sprintf "(%s)" (string_of_val' d conenv t v)

and string_of_val' d conenv t v =
  match Type.normalize conenv t with
  | T.Opt t ->
    (match v with
    | Null -> "null"
    | _ -> string_of_val' d conenv t v
    )
  | T.Async t ->
    let {result; waiters} = as_async v in
    sprintf "async %s" (string_of_def_nullary d conenv t result)
  | T.Like t -> 
    sprintf "like %s" (T.string_of_typ t) (* TBR *)
  | T.Obj (T.Actor, fs) ->
    sprintf "actor %s" (string_of_val_nullary d conenv (T.Obj (T.Object, fs)) v)
  | T.Mut t ->
    string_of_val' d conenv t !(as_mut v)
  | _ -> string_of_val_nullary d conenv t v

and string_of_def_nullary d conenv t def =
  match Lib.Promise.value_opt def with
  | Some v -> string_of_val_nullary d conenv t v
  | None -> "_"

and string_of_def' d conenv t def =
  match Lib.Promise.value_opt def with
  | Some v -> string_of_val' d conenv t v
  | None -> "_"

let string_of_val conenv t v = string_of_val' !Flags.print_depth conenv t v
let string_of_def conenv t d = string_of_def' !Flags.print_depth conenv t d


(* Debug pretty printing *)

let rec debug_string_of_val_nullary d = function
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Nat n -> Nat.to_string n
  | Int i -> Int.to_string i
  | Word8 w -> Word8.to_string_u w
  | Word16 w -> Word16.to_string_u w
  | Word32 w -> Word32.to_string_u w
  | Word64 w -> Word64.to_string_u w
  | Float f -> Float.to_string f
  | Char c -> string_of_char c
  | Text t -> string_of_text t
  | Tup vs ->
    sprintf "(%s)" (String.concat ", " (List.map (debug_string_of_val' d) vs))
  | Obj ve ->
    if d = 0 then "{...}" else
    sprintf "{%s}" (String.concat "; " (List.map (fun (x, v) ->
      sprintf "%s = %s" x (debug_string_of_val' (d - 1) v)) (Env.bindings ve)))
  | Array a ->
    sprintf "[%s]" (String.concat ", "
      (List.map (debug_string_of_val' d) (Array.to_list a)))
  | Func f -> "func"
  | v -> "(" ^ debug_string_of_val' d v ^ ")"

and debug_string_of_val' d = function
  | Async {result; waiters} ->
    sprintf "async %s #%i" (debug_string_of_def_nullary d result)
      (List.length waiters)
  | Mut r -> sprintf "%s" (debug_string_of_val' d !r)
  | v -> debug_string_of_val_nullary d v

and debug_string_of_def_nullary d def =
  match Lib.Promise.value_opt def with
  | Some v -> debug_string_of_val_nullary d v
  | None -> "_"

and debug_string_of_def' d def =
  match Lib.Promise.value_opt def with
  | Some v -> debug_string_of_val' d v
  | None -> "_"

let debug_string_of_val v = debug_string_of_val' !Flags.print_depth v
let debug_string_of_def d = debug_string_of_def' !Flags.print_depth d


let debug_string_of_tuple_val = function
  | Tup _ as v -> debug_string_of_val v
  | v -> "(" ^ debug_string_of_val v ^ ")"
