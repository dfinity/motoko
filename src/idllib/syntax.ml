
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
  | Reserved        
        
type func_mode = func_mode' Source.phrase
and func_mode' = Oneway | Pure

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
and typ_field' = { id : int32; name : id; typ : typ }

and typ_meth = typ_meth' Source.phrase
and typ_meth' = {var : id; meth : typ}

(* Declarations *)

and dec = dec' Source.phrase
and dec' =
  | TypD of id * typ             (* type *)
  | ImportD of string * string ref  (* import *)

and actor = actor' Source.phrase
and actor' = 
  | ActorD of id * typ     (* service *)
               
(* Program *)

type prog = (prog', string) Source.annotated_phrase
and prog' = { decs : dec list; actor : actor option }

(* Libraries *)
          
type library = string * prog
type libraries = library list                          
