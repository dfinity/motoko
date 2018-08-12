(* Numeric Representations *)

(*
module Int32Rep : Wasm.Int.RepType with type t = int32
module Int16Rep : Wasm.Int.RepType with type t = int32
module Int8Rep : Wasm.Int.RepType with type t = int32
*)

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
end

module type NumType =
sig
  include module type of Z
  val sub : t -> t -> t
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
end

module Word8 : WordType with type bits = int32
module Word16 : WordType with type bits = int32
module Word32 : WordType with type bits = int32 and type t = Wasm.I32.t
module Word64 : WordType with type bits = int64 and type t = Wasm.I64.t
module Float : Wasm.Float.S with type bits = int64 and type t = Wasm.F64.t

module Nat : NumType
module Int : NumType


(* Environment *)

module Env : Env.S with type key = string


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
and rec_bind = Rec of recursive
and recursive = {mutable def : bind option}


(* Projections *)

val unit : value

val as_null : value -> unit
val as_bool : value -> bool
val as_nat : value -> Nat.t
val as_int : value -> Int.t
val as_word8 : value -> Word8.t
val as_word16 : value -> Word16.t
val as_word32 : value -> Word32.t
val as_word64 : value -> Word64.t
val as_float : value -> Float.t
val as_char : value -> unicode
val as_text : value -> string
val as_array : value -> value array
val as_tup : value -> value list
val as_obj : value -> rec_bind Env.t
val as_opt : value -> value option
val as_func : value -> (value -> cont -> value)
val as_async : value -> async

val as_val_bind : bind -> value
val as_var_bind : bind -> value ref
val as_rec_bind : rec_bind -> recursive

val read_bind : bind -> value
val read_rec_bind : rec_bind -> value
val unroll_rec_bind : rec_bind -> bind

(*
val nullV = NullV
val null_of_V (NullV) = ()
let boolV b = BoolV b
let bool_of_V (BoolV b) = b
let natV n = NatV n
let nat_of_V (NatV n) = n
let intV n = IntV n
let int_of_V (IntV n) = n
let word8V w = Word8V w
let word8_of_V (Word8V w) = w
let word16V w = Word16V w
let word16_of_V (Word16V w) = w
let word32V w = Word32V w
let word32_of_V (Word32V w) = w
let word64V w = Word64V w
let word64_of_V (Word64V w) = w

let floatV f = FloatV f
let float_of_V (FloatV f) = f
let charV c = CharV c
let char_of_V (CharV c) = c
let textV s = TextV s
let text_of_V (TextV s) = s
let arrV a = ArrV a
let arr_of_V (ArrV a) = a
let tupV vs = TupV vs
let tup_of_V (TupV vs) = vs
let objV ve = ObjV ve
let obj_of_V (ObjV ve) = ve
let optV ve = OptV ve
let opt_of_V (OptV v) = v
let funcV f = FuncV f
let func_of_V (FuncV f) = f
let unitV = TupV([])
let asyncV async = AsyncV async


let varB r = VarB r
let rec derefV v =
    match v with
    | VarB r -> !r

let valB v = ValB v
let rec val_of_B v =
    match v with
    | ValB v -> v

let recR d = RecR {definition=d} 
let rec_of_R (RecR r) = r
let checkV v =
     match v with
     | RecR r ->
        (match r.definition with
         | Some v -> v
       | None -> failwith "BlackHole" (*TBR*))

let projV (TupV vs) n = List.nth vs (Int32.to_int n)
let dotV (ObjV ve) v = Env.find v ve
let assignV (VarB r) v  = r := v;unitV
let updateV (ArrV a) (IntV i) v  = a.(Int32Rep.to_int i) <- v;unitV (* TBR *)
let indexV (ArrV a) (IntV i) = a.(Int32Rep.to_int i) (*TBR*)
let applyV (FuncV f) v k = f v k

let notV (BoolV b) = BoolV (not b)
let async_of_V(AsyncV async) = async
let set_result async v =
    match async with
    | {result=None;waiters=waiters} ->
        async.result <- Some v;
        async.waiters <- [];
        List.fold_left (fun runnables waiter -> (fun () -> waiter v)::runnables) [] waiters; 
    | {result=Some _} -> failwith "set_result"
let get_result async k =
    match async with
    | {result=Some v} -> k v
        | {result=None;waiters} -> (async.waiters <- k::waiters; unitV)
*)


(* Pretty Printing *)

val string_of_val : Type.kind Con.Env.t -> Type.typ -> value -> string

val debug_string_of_val : value -> string
val debug_string_of_bind : bind -> string
val debug_string_of_rec_bind : rec_bind -> string
val debug_string_of_tuple_val : value -> string
