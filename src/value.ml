open Printf
module T = Type

(* Environments *)

module Env = Env.Make(String)


(* Numeric Representations *)

let rec add_digits buf s i j k =
  if i < j then begin
    if k = 0 then Buffer.add_char buf '_';
    Buffer.add_char buf s.[i];
    add_digits buf s (i + 1) j ((k + 2) mod 3)
  end

let is_digit c = '0' <= c && c <= '9'
let isnt_digit c = not (is_digit c)

let group_num s =
  let len = String.length s in
  let mant = Lib.Option.get (Lib.String.find_from_opt is_digit s 0) len in
  let point = Lib.Option.get (Lib.String.find_from_opt isnt_digit s mant) len in
  let frac = Lib.Option.get (Lib.String.find_from_opt is_digit s point) len in
  let exp = Lib.Option.get (Lib.String.find_from_opt isnt_digit s frac) len in
  let buf = Buffer.create (len*4/3) in
  Buffer.add_substring buf s 0 mant;
  add_digits buf s mant point ((point - mant) mod 3 + 3);
  Buffer.add_substring buf s point (frac - point);
  add_digits buf s frac exp 3;
  Buffer.add_substring buf s exp (len - exp);
  Buffer.contents buf

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
  let shift_right i j = let res = Rep.shift_right i j in inj (proj res)
  let shift_right_logical i j = let res = Rep.shift_right_logical i j in inj (proj res)
  let of_int i = inj (Rep.of_int i)
  let to_int i = Rep.to_int (proj i)
  let to_string i = group_num (Rep.to_string (proj i))
end

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
  val pow : t -> t -> t
  val to_string : t -> string
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
  let to_string w = group_num (WasmInt.to_string_u w)
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
  let to_string w = group_num (WasmFloat.to_string w)
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
  let to_string i = group_num (string_of_big_int i)
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

type call_conv = {
  sort: Type.sharing;
  control : Type.control;
  n_args : int;
  n_res : int;
}

let call_conv_of_typ typ =
  match typ with
  | Type.Func(sort, control, tbds, dom, res) ->
    { sort; control; n_args = List.length dom; n_res = List.length res }
  | _ -> raise (Invalid_argument ("call_conv_of_typ " ^ T.string_of_typ typ))

type func =
  (value -> value cont -> unit)
and value =
  | Null
  | Bool of bool
  | Int of Int.t
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Tup of value list
  | Opt of value
  | Variant of string * value
  | Array of value array
  | Obj of value Env.t
  | Func of call_conv * func
  | Async of async
  | Mut of value ref
  | Serialized of value

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Smart constructors *)

let local_cc n m = { sort = T.Local; control = T.Returns; n_args = n; n_res = m}
let message_cc n = { sort = T.Sharable; control = T.Returns; n_args = n; n_res = 0}
let async_cc n = { sort = T.Sharable; control = T.Promises; n_args = n; n_res = 1}

let local_func n m f = Func (local_cc n m, f)
let message_func n f = Func (message_cc n, f)
let async_func n f = Func (async_cc n, f)


(* Projections *)

let invalid s = raise (Invalid_argument ("Value." ^ s))

let as_null = function Null -> () | _ -> invalid "as_null"
let as_bool = function Bool b -> b | _ -> invalid "as_bool"
let as_int = function Int n -> n | _ -> invalid "as_int"
let as_word8 = function Word8 w -> w | _ -> invalid "as_word8"
let as_word16 = function Word16 w -> w | _ -> invalid "as_word16"
let as_word32 = function Word32 w -> w | _ -> invalid "as_word32"
let as_word64 = function Word64 w -> w | _ -> invalid "as_word64"
let as_float = function Float f -> f | _ -> invalid "as_float"
let as_char = function Char c -> c | _ -> invalid "as_char"
let as_text = function Text s -> s | _ -> invalid "as_text"
let as_array = function Array a -> a | _ -> invalid "as_array"
let as_opt = function Opt v -> v | _ -> invalid "as_opt"
let as_variant = function | Variant (i, v) -> i, v | _ -> invalid "as_variant"
let as_tup = function Tup vs -> vs | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [v1; v2] -> v1, v2 | _ -> invalid "as_pair"
let as_serialized = function Serialized v -> v | _ -> invalid "as_serialized"

let obj_of_array a =
  let get = local_func 1 1 @@ fun v k ->
    let n = as_int v in
    if Nat.lt n (Nat.of_int (Array.length a)) then
      k (a.(Nat.to_int n))
    else
      raise (Invalid_argument "array index out of bounds") in

  let set = local_func 2 0 @@ fun v k ->
    let v1, v2 = as_pair v in
    let n = as_int v1 in
    if Nat.lt n (Nat.of_int (Array.length a)) then
      k (a.(Nat.to_int n) <- v2; Tup [])
    else
      raise (Invalid_argument "array index out of bounds") in

  let len = local_func 0 1 @@ fun v k ->
    as_unit v; k (Int (Nat.of_int (Array.length a))) in

  let keys = local_func 0 1 @@ fun v k ->
    as_unit v;
    let i = ref 0 in
    let next = local_func 0 1 @@ fun v k' ->
        if !i = Array.length a then k' Null else
          let v = Opt (Int (Nat.of_int !i)) in incr i; k' v
    in k (Obj (Env.singleton "next" next)) in

  let vals = local_func 0 1 @@ fun v k ->
    as_unit v;
    let i = ref 0 in
    let next = local_func 0 1 @@ fun v k' ->
        if !i = Array.length a then k' Null else
          let v = Opt (a.(!i)) in incr i; k' v
    in k (Obj (Env.singleton "next" next)) in

  Env.from_list ["get", get; "set", set; "len", len; "keys", keys; "vals", vals]

let obj_of_text t =
  let chars = local_func 0 1 @@ fun v k ->
    as_unit v;
    let i = ref 0 in
    let s = Wasm.Utf8.decode t in
    let next = local_func 0 1 @@ fun v k' ->
        if !i = List.length s then k' Null else
          let v = Opt (Char (List.nth s !i)) in incr i; k' v
    in k (Obj (Env.singleton "next" next)) in
  let len = local_func 0 1 @@ fun v k ->
    as_unit v; k (Int (Nat.of_int (List.length (Wasm.Utf8.decode t)))) in

  Env.from_list ["chars", chars; "len", len]

let as_obj = function Obj ve -> ve | Array a -> obj_of_array a | Text t -> obj_of_text t | _ -> invalid "as_obj"
let as_func = function Func (cc, f) -> cc, f | _ -> invalid "as_func"
let as_async = function Async a -> a | _ -> invalid "as_async"
let as_mut = function Mut r -> r | _ -> invalid "as_mut"


(* Primitives *)

let unit = Tup []


(* Ordering *)

let generic_compare = compare

let rec compare x1 x2 =
  if x1 == x2 then 0 else
  match x1, x2 with
  | Int n1, Int n2 -> Int.compare n1 n2
  | Opt v1, Opt v2 -> compare v1 v2
  | Tup vs1, Tup vs2 -> Lib.List.compare compare vs1 vs2
  | Array a1, Array a2 -> Lib.Array.compare compare a1 a2
  | Obj fs1, Obj fs2 -> Env.compare compare fs1 fs2
  | Mut r1, Mut r2 -> compare !r1 !r2
  | Async _, Async _ -> raise (Invalid_argument "Value.compare")
  | _ -> generic_compare x1 x2

let equal x1 x2 = compare x1 x2 = 0


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
  | Int i -> Int.to_string i
  | Word8 w -> Word8.to_string w
  | Word16 w -> Word16.to_string w
  | Word32 w -> Word32.to_string w
  | Word64 w -> Word64.to_string w
  | Float f -> Float.to_string f
  | Char c -> string_of_string '\'' [c] '\''
  | Text t -> string_of_string '\"' (Wasm.Utf8.decode t) '\"'
  | Tup vs ->
    sprintf "(%s%s)"
      (String.concat ", " (List.map (string_of_val' d) vs))
      (if List.length vs = 1 then "," else "")
  | Opt v ->
    sprintf "?%s" (string_of_val_nullary d v)
  | Obj ve ->
    if d = 0 then "{...}" else
    sprintf "{%s}" (String.concat "; " (List.map (fun (x, v) ->
      sprintf "%s = %s" x (string_of_val' (d - 1) v)) (Env.bindings ve)))
  | Array a ->
    sprintf "[%s]" (String.concat ", "
      (List.map (string_of_val' d) (Array.to_list a)))
  | Func (_, _) -> "func"
  | v -> "(" ^ string_of_val' d v ^ ")"

and string_of_val' d = function
  | Async {result; waiters = []} ->
    sprintf "async %s" (string_of_def_nullary d result)
  | Async {result; waiters} ->
    sprintf "async[%d] %s"
      (List.length waiters) (string_of_def_nullary d result)
  | Variant (l, Tup[]) ->
    sprintf "#%s" l
  | Variant (l, v) ->
    sprintf "#%s %s" l (string_of_val_nullary d v)
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

let string_of_call_conv {sort;control;n_args;n_res} =
  sprintf "(%s %i %s %i)"
    (T.string_of_sharing sort)
    n_args
    (match control with
     | T.Returns -> "->"
     | T.Promises -> "@>")
    n_res
