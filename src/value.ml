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

module Int32Rep = struct include Int32 let bitwidth = 32 end
module Int16Rep = SubRep(Int32Rep)(struct let bitwidth = 16 end)
module Int8Rep = SubRep(Int32Rep)(struct let bitwidth = 8 end)

module Word8 = Wasm.Int.Make(Int8Rep)
module Word16 = Wasm.Int.Make(Int16Rep)
module Word32 = Wasm.I32
module Word64 = Wasm.I64

module Nat = Wasm.Int.Make(struct include Int64 let bitwidth = 64 end)
module Int = Wasm.Int.Make(struct include Int64 let bitwidth = 64 end)
module Float = Wasm.F64

let nat_width = 64
let int_width = 64


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
  | Obj of rec_bind Env.t
  | Array of value array
  | Opt of value option (* TBR *)
  | Func of (value -> cont -> value)
  | Async of async

and async = {mutable result: value option; mutable waiters : cont list}

and cont = value -> value

and bind = 
  | Val of value
  | Var of value ref
and rec_bind =
  | Rec of recursive
(*TBR: we could statically distinguish lambda-pat bound variables from other binds to avoid
       the unnessary indirection and definedness check for references to lambda-bound variables, in which
    case we could add:
  | Val of value 
*)
and recursive = {mutable def : bind option}

let unit = Tup []

let invalid s = raise (Invalid_argument s)

let as_null = function Null -> () | _ -> invalid "as_null"
let as_bool = function Bool b -> b | _ -> invalid "as_bool"
let as_nat = function Nat n -> n | _ -> invalid "as_nat"
let as_int = function Int n -> n | _ -> invalid "as_int"
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
let as_opt = function Opt vo -> vo | _ -> invalid "as_opt"
let as_func = function Func f -> f | _ -> invalid "as_func"
let as_async = function Async a -> a | _ -> invalid "as_async"

let as_val_bind = function Val v -> v | _ -> invalid "as_val_bind"
let as_var_bind = function Var r -> r | _ -> invalid "as_var_bind"
let as_rec_bind = function Rec r -> r (*| _ -> invalid "as_rec_bind"*)

let read_bind = function
  | Val v -> v
  | Var r -> !r

let unroll_rec_bind = function
  | Rec {def = Some v} -> v
  | _ -> failwith "BlackHole" (* TBR *)

let read_rec_bind b = read_bind (unroll_rec_bind b)


(* Pretty Printing *)

let string_of_mut = function
  | Type.Const -> ""
  | Type.Mut -> "var "

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

let rec string_of_val_nullary conenv t v =
  match T.normalize conenv t with
  | T.Any -> "any"
  | T.Prim T.Null -> as_null v; "null"
  | T.Prim T.Bool -> if as_bool v then "true" else "false"
  | T.Prim T.Nat -> Nat.to_string_u (as_nat v)
  | T.Prim T.Int ->  Int.to_string_s (as_int v)
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
      (String.concat ", " (List.map2 (string_of_val conenv) ts vs))
  | T.Array (m, t) ->
    let a = as_array v in
    sprintf "[%s%s]"
      (string_of_mut m)
      (String.concat ", " (List.map (string_of_val conenv t) (Array.to_list a)))
  | T.Obj (T.Object, fs) ->
    let ve = as_obj v in
    sprintf "{%s}"
      (String.concat "; " (List.map (fun {T.lab; mut; typ} ->
        let b = unroll_rec_bind (Env.find lab ve) in
        let v = match mut with
          | T.Mut -> !(as_var_bind b)
          | T.Const -> as_val_bind b
        in sprintf "%s%s = %s" (T.string_of_mut mut) lab (string_of_val conenv typ v)
      ) fs))
  | Func _ ->
    ignore (as_func v); (* catch errors *)
    "func"
  | _ ->
    sprintf "(%s)" (string_of_val conenv t v)

and string_of_val conenv t v =
  match Type.normalize conenv t with
  | T.Opt t ->
    let v = as_opt v in
    (match v with
    | None -> "null"
    | Some v -> string_of_val conenv t v
    )
  | T.Async t ->
    let {result; waiters} = as_async v in
    sprintf "async %s#%i"
      (match result with
      | None -> "?"
      | Some v -> string_of_val_nullary conenv t v
      )
      (List.length waiters)
  | T.Like t -> 
    sprintf "like %s" (T.string_of_typ t) (* TBR *)
  | T.Obj (T.Actor, fs) ->
    sprintf "actor %s" (string_of_val_nullary conenv (T.Obj (T.Object, fs)) v)
  | _ -> string_of_val_nullary conenv t v


(* Debug pretty printing *)

let rec debug_string_of_val_nullary = function
  | Null  -> "null"
  | Bool b -> if b then "true" else "false"
  | Nat n -> Nat.to_string_u n
  | Int i -> Int.to_string_s i
  | Word8 w -> Word8.to_string_u w
  | Word16 w -> Word16.to_string_u w
  | Word32 w -> Word32.to_string_u w
  | Word64 w -> Word64.to_string_u w
  | Float f -> Float.to_string f
  | Char c -> string_of_char c
  | Text t -> string_of_text t
  | Tup vs ->
    sprintf "(%s)" (String.concat ", " (List.map (debug_string_of_val) vs))
  | Obj ve ->
    sprintf "{%s}" (String.concat "; " (List.map (fun (v, w) ->
  	  sprintf "%s = %s" v (debug_string_of_rec_bind w)) (Env.bindings ve)))
  | Array a ->
    sprintf "[%s]"
      (String.concat ", " (List.map debug_string_of_val (Array.to_list a)))
  | Opt o ->
    (match o with
    | None -> "null"
    | Some v -> debug_string_of_val_nullary v
    )
  | Func f -> "func"
  | v -> "(" ^ debug_string_of_val v ^ ")"

and debug_string_of_val = function
  | Opt None -> "null"
  | Opt (Some v) -> debug_string_of_val v
  | Async {result; waiters} ->
    sprintf "async %s#%i"
      (match result with
      | None -> "?"
      | Some v -> debug_string_of_val_nullary v
      )
      (List.length waiters)
  | v -> debug_string_of_val_nullary v

and debug_string_of_bind = function
  | Var r -> 
    (*sprintf "Var (%s)"*) (debug_string_of_val !r) (*TBR show address ?*) 
  | Val v ->
    debug_string_of_val v

and debug_string_of_rec_bind = function
  | Rec {def = Some bind} -> 
    (*sprintf "Rec (%s)"*) (debug_string_of_bind bind) 
  | Rec {def = None} -> 
    "Rec (?)" 


let debug_string_of_tuple_val = function
  | Tup _ as v -> debug_string_of_val v
  | v -> "(" ^ debug_string_of_val v ^ ")"
