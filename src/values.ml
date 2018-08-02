open Syntax
open Source
open Types
open Printf
(*TBR*)

type value =
    | NullV 
    | BoolV of bool
    | NatV of Natural.t
    | IntV of Integer.t
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
and recursive = {mutable definition: binding option}
and recbinding = 
    | RecR of recursive
(*TBR: we could statically distinguish lambda-pat bound variables from other bindings to avoid
       the unnessary indirection and definedness check for references to lambda-bound variables, in which
    case we could add:
    | ValR of value 
*)

let nullV = NullV
let null_of_V (NullV) = ()
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

(* dynamic pretty printing *)
let rec debug_string_of_atomic_val = function
    | NullV  -> "null"
    | BoolV b -> if b then "true" else "false"
    | NatV n -> Natural.to_string_u n
    | IntV i -> Integer.to_string_s i
    | Word8V w -> Word8.to_string_u w
    | Word16V w -> Word16.to_string_u w
    | Word32V w -> Word32.to_string_u w
    | Word64V w -> Word64.to_string_u w
    | FloatV f -> Float.to_string f
    | CharV d -> sprintf "'\\u%lx'" d (* TBR *)
    | TextV t -> "\"" ^ String.escaped t ^ "\"" (* TBR *)
    | TupV vs -> sprintf "(%s)" (String.concat ", " (List.map (debug_string_of_val) vs))
    | ObjV ve -> sprintf "{%s}" (String.concat "; " (List.map (fun (v,w) ->
    	              	                  sprintf "%s = %s" v (debug_string_of_recbind w)) (Env.bindings ve)))
    | ArrV a ->
       sprintf "[%s]" (String.concat ", " (List.map debug_string_of_val (Array.to_list a)))
    | OptV o ->
      (match o with
      | None -> "null"
      | Some v -> debug_string_of_atomic_val v)
    | v -> "(" ^ debug_string_of_val v ^ ")"

and debug_string_of_val = function
    | OptV o ->
      (match o with
      | None -> "null"
      | Some v -> debug_string_of_val v)
    | FuncV f -> "func"
    | AsyncV async ->
      let {result; waiters} = async in
      sprintf "async %s#%i"
        (match result with None -> "?" | Some v -> debug_string_of_atomic_val v)
        (List.length waiters)
    | v -> debug_string_of_atomic_val v

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

