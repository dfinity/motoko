open Printf
(*TBR*)

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

type nat = Nat.t
type unicode = int32

type word =
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t

type value =
  | NullV 
  | BoolV of bool
  | NatV of Nat.t
  | IntV of Int.t
  | Word8V of Word8.t
  | Word16V of Word16.t
  | Word32V of Word32.t
  | Word64V of Word64.t
  | FloatV of Float.t
  | CharV of unicode
  | TextV of string
  | TupV of value list
  | ObjV of recbinding Env.t
  | ArrV of value array
  | OptV of value option (* TBR *)
  | FuncV of (value -> cont -> value)
  | AsyncV of async

and async = {mutable result: value option; mutable waiters : cont list}

and cont = value -> value

and binding = 
  | ValB of value
  | VarB of value ref
and recursive = {mutable definition : binding option}
and recbinding =
  | RecR of recursive
(*TBR: we could statically distinguish lambda-pat bound variables from other bindings to avoid
       the unnessary indirection and definedness check for references to lambda-bound variables, in which
    case we could add:
  | ValR of value 
*)

(*
let nullV = NullV
let boolV b = BoolV b
let natV n = NatV n
let intV n = IntV n
let word8V w = Word8V w
let word16V w = Word16V w
let word32V w = Word32V w
let word64V w = Word64V w
let floatV f = FloatV f
let charV c = CharV c
let textV s = TextV s
let arrV a = ArrV a
let tupV vs = TupV vs
let objV ve = ObjV ve
let optV vo = OptV vo
let funcV f = FuncV f
let asyncV async = AsyncV async
*)

let unitV = TupV []

let invalid s = raise (Invalid_argument s)

let as_null = function NullV -> () | _ -> invalid "as_null"
let as_bool = function BoolV b -> b | _ -> invalid "as_bool"
let as_nat = function NatV n -> n | _ -> invalid "as_nat"
let as_int = function IntV n -> n | _ -> invalid "as_int"
let as_word8 = function Word8V w -> w | _ -> invalid "as_word8"
let as_word16 = function Word16V w -> w | _ -> invalid "as_word16"
let as_word32 = function Word32V w -> w | _ -> invalid "as_word32"
let as_word64 = function Word64V w -> w | _ -> invalid "as_word64"
let as_float = function FloatV f -> f | _ -> invalid "as_float"
let as_char = function CharV c -> c | _ -> invalid "as_char"
let as_text = function TextV s -> s | _ -> invalid "as_text"
let as_arr = function ArrV a -> a | _ -> invalid "as_arr"
let as_tup = function TupV vs -> vs | _ -> invalid "as_tup"
let as_obj = function ObjV ve -> ve | _ -> invalid "as_obj"
let as_opt = function OptV vo -> vo | _ -> invalid "as_opt"
let as_func = function FuncV f -> f | _ -> invalid "as_func"
let as_async = function AsyncV a -> a | _ -> invalid "as_async"


(*
let varB r = VarB r
let valB v = ValB v
let recR d = RecR {definition = d} 
*)
let as_var_bind = function VarB r -> r | _ -> invalid "as_var_bind"
let as_val_bind = function ValB v -> v | _ -> invalid "as_val_bind"
let as_rec_bind = function RecR r -> r (*| _ -> invalid "as_rec_bind"*)
let unroll_rec_bind = function
  | RecR {definition = Some v} -> v
  | _ -> failwith "BlackHole" (* TBR *)


(* Pretty Printing *)

let string_of_mut = function
  | Type.Const -> ""
  | Type.Mut -> "var "

let rec string_of_val_nullary conenv t v =
  let module ValueEnv = Env in
  let open Type in
  match Type.normalize conenv t with
  | Any -> "any"
  | Prim Null -> as_null v; "null"
  | Prim Int ->  Int.to_string_s (as_int v)
  | Prim Bool -> if as_bool v then "true" else "false"
  | Prim Float -> Float.to_string (as_float v)
  | Prim Nat -> Nat.to_string_u (as_nat v)
  | Prim Char -> sprintf "'\\u%lx'" (as_char v) (* TBR *)
  | Prim (Word Width8) -> Word8.to_string_u (as_word8 v)
  | Prim (Word Width16) -> Word16.to_string_u (as_word16 v)
  | Prim (Word Width32) -> Word32.to_string_u (as_word32 v)
  | Prim (Word Width64) -> Word64.to_string_u (as_word64 v)
  | Prim Text -> "\"" ^ String.escaped (as_text v) ^ "\"" (* TBR *)
  | Var (c, []) -> Con.to_string c
  | Var (c, ts) ->
    sprintf "%s<%s>"
      (Con.to_string c)
      (String.concat ", " (List.map Type.string_of_typ ts))
  | Tup ts ->
    let vs = as_tup v in
    sprintf "(%s)"
      (String.concat ", " (List.map2 (string_of_val conenv) ts vs))
  | Array (m, t) ->
    let a = as_arr v in
    sprintf "[%s%s]"
      (string_of_mut m)
      (String.concat ", " (List.map (string_of_val conenv t) (Array.to_list a)))
  | Obj (Object, fs) ->
    let ve = as_obj v in
    sprintf "{%s}"
      (String.concat "; " (List.map (fun {lab; mut; typ} ->
        let b = unroll_rec_bind (ValueEnv.find lab ve) in
        let v = match mut with
          | Mut -> !(as_var_bind b)
          | Const -> as_val_bind b
        in sprintf "%s%s = %s" (string_of_mut mut) lab (string_of_val conenv typ v)
      ) fs))
  | Func _ ->
    ignore (as_func v); (* catch errors *)
    "func"
  | _ ->
    sprintf "(%s)" (string_of_val conenv t v)

and string_of_val conenv t v =
  let open Type in
  match Type.normalize conenv t with
  | Opt t ->
    let v = as_opt v in
    (match v with
    | None -> "null"
    | Some v -> string_of_val conenv t v
    )
  | Async t ->
    let {result; waiters} = as_async v in
    sprintf "async %s#%i"
      (match result with
      | None -> "?"
      | Some v -> string_of_val_nullary conenv t v
      )
      (List.length waiters)
  | Like t -> 
    sprintf "like %s" (Type.string_of_typ t) (* TBR *)
  | Obj (Actor, fs) ->
    sprintf "actor %s" (string_of_val_nullary conenv (Obj (Object, fs)) v)
  | _ -> string_of_val_nullary conenv t v


(* Debug pretty printing *)

let rec debug_string_of_val_nullary = function
  | NullV  -> "null"
  | BoolV b -> if b then "true" else "false"
  | NatV n -> Nat.to_string_u n
  | IntV i -> Int.to_string_s i
  | Word8V w -> Word8.to_string_u w
  | Word16V w -> Word16.to_string_u w
  | Word32V w -> Word32.to_string_u w
  | Word64V w -> Word64.to_string_u w
  | FloatV f -> Float.to_string f
  | CharV d -> sprintf "'\\u%lx'" d (* TBR *)
  | TextV t -> "\"" ^ String.escaped t ^ "\"" (* TBR *)
  | TupV vs ->
    sprintf "(%s)" (String.concat ", " (List.map (debug_string_of_val) vs))
  | ObjV ve ->
    sprintf "{%s}" (String.concat "; " (List.map (fun (v, w) ->
  	  sprintf "%s = %s" v (debug_string_of_recbind w)) (Env.bindings ve)))
  | ArrV a ->
    sprintf "[%s]"
      (String.concat ", " (List.map debug_string_of_val (Array.to_list a)))
  | OptV o ->
    (match o with
    | None -> "null"
    | Some v -> debug_string_of_val_nullary v
    )
  | FuncV f -> "func"
  | v -> "(" ^ debug_string_of_val v ^ ")"

and debug_string_of_val = function
  | OptV None -> "null"
  | OptV (Some v) -> debug_string_of_val v
  | AsyncV {result; waiters} ->
    sprintf "async %s#%i"
      (match result with
      | None -> "?"
      | Some v -> debug_string_of_val_nullary v
      )
      (List.length waiters)
  | v -> debug_string_of_val_nullary v

and debug_string_of_bind = function
  | VarB r -> 
    (*sprintf "Var (%s)"*) (debug_string_of_val !r) (*TBR show address ?*) 
  | ValB v ->
    debug_string_of_val v

and debug_string_of_recbind = function
  | RecR {definition = Some bind} -> 
    (*sprintf "Rec (%s)"*) (debug_string_of_bind bind) 
  | RecR {definition = None} -> 
    "Rec (?)" 


let debug_string_of_tuple_val = function
  | TupV _ as v -> debug_string_of_val v
  | v -> "(" ^ debug_string_of_val v ^ ")"

