open Syntax
open Source
open Types
open Printf
open Typing
open Values
(*TBR*)


let rec string_of_mut = function
  | ConstMut -> ""
  | VarMut -> "var "

let rec string_of_atomic_val context t v =
  match norm_typ context t with
  | AnyT -> "any"
  | PrimT p ->
    (match p with
    | NullT -> let _ = null_of_V v in "null"
    | IntT ->  Integer.to_string_s (int_of_V v)
    | BoolT -> if (bool_of_V v) then "true" else "false"
    | FloatT -> Float.to_string (float_of_V v)
    | NatT -> Natural.to_string_u (nat_of_V v)
    | CharT -> sprintf "%i" (Int32.to_int(char_of_V v)) (* TBR *)
    | WordT Width8 ->
        let w = word8_of_V v in
        Word8.to_string_u w
    | WordT Width16 ->
        let w = word16_of_V v in
        Word16.to_string_u w
    | WordT Width32 ->
        let w = word32_of_V v in
        Word32.to_string_u w
    | WordT Width64->
        let w = word64_of_V v in
        Word64.to_string_u w
    | TextT -> text_of_V v)
  | VarT (c,[]) ->
     Con.to_string c
  | VarT (c,ts) ->
     sprintf "%s<%s>" (Con.to_string c) (String.concat ", " (List.map string_of_typ ts))
  | TupT ts ->
    let vs = tup_of_V v in
    sprintf "(%s)"  (String.concat ", " (List.map2 (string_of_val context) ts vs))
  | ArrayT (m,t) ->
    let a = arr_of_V v in
    sprintf "[%s%s]" (string_of_mut m)
             (String.concat ", " (List.map (string_of_val context t) (Array.to_list a)))
  | ObjT(Object,fs) ->
    let ve = obj_of_V v in
    sprintf "{%s}" (String.concat "; " (List.map (fun {var;mut;typ} ->
    	              	                 let v = checkV (Env.find var ve) in
					 let v = match mut with
					         | VarMut -> derefV v
						 | ConstMut -> v
				         in
                                       sprintf "%s%s = %s" (string_of_mut mut) var (string_of_atomic_val context typ v))
                    fs))
  | _ ->
    sprintf "(%s)" (string_of_val context t v)

and string_of_val context t v =
  match norm_typ context t with
  | FuncT(_,_,_) ->
    ignore (func_of_V v); (* catch errors *)
    "func ..."
  | OptT t ->
    let v = opt_of_V v in
    (match v with
    | None -> "null"
    | Some v -> string_of_val context t v)
  | AsyncT t ->
    let {result;waiters} = async_of_V v in
    sprintf "async {%s, %i}" (match result with
                            | None -> "?"
           		      | Some v -> string_of_val context t v)
			      (List.length waiters)
  | LikeT t -> 
    sprintf "like %s" (string_of_atomic_typ t) (* TBR *)
  | ObjT(Actor,fs) ->
    sprintf "actor %s" (string_of_atomic_val context (ObjT(Object,fs)) v)
  | _ -> string_of_atomic_val context t v



  
