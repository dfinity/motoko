
(* Identifiers *)

type id = string Source.phrase

(* Types *)

type prim =
  | Nat
  | Nat8
  | Nat16
  | Nat32
  | Nat64
  | Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | Bool
  | Text
  | Null
  | Unavailable        
        
type func_mode = func_mode' Source.phrase
and func_mode' = Sensitive | Pure

type typ = typ' Source.phrase
and typ' =
  | PrimT of prim                                (* primitive *)
  | VarT of id                                    (* type name *)
  | FuncT of func_mode list * typ_field list * typ_field list   (* function *)
  | OptT of typ   (* option *)
  | VecT of typ   (* vector *)
  | RecordT of typ_field list  (* record *)
  | VariantT of typ_field list (* variant *)
  | ServT of typ_meth list (* service reference *)
  | PreT   (* pre-type *)

and typ_field = typ_field' Source.phrase
and typ_field' = { id : Stdint.uint64; name : id; typ : typ }

and typ_meth = typ_meth' Source.phrase
and typ_meth' = {var : id; meth : typ}

(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | TypD of id * typ             (* type *)

and actor = actor' Source.phrase
and actor' = 
  | ActorD of id * typ_meth list     (* service *)
  | ActorVarD of id * id  (* service reference *)
               
(* Program *)

type prog = prog' Source.phrase
and prog' = { decs : dec list; actor : actor option }
