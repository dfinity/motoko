module Type = Type_idl

(* Notes *)

type typ_note = {note_typ : Type.typ}

let empty_typ_note = {note_typ = Type.Pre}


(* Identifiers *)

type id = string Source.phrase

(* Types *)

type func_mode = Type.mode Source.phrase
                 
type typ = (typ', Type.typ) Source.annotated_phrase
and typ' =
  | PrimT of string                                (* primitive *)
  | VarT of id                                    (* type name *)
  | FuncT of func_mode list * typ * typ   (* function *)
  | TupT of typ list (* tuple *)
  | OptT of typ   (* option *)
  | VecT of typ   (* vector *)
  | RecordT of typ_field list  (* record *)
  | VariantT of typ_field list (* variant *)
  | RefFuncT of typ   (* function reference *)
  | RefServT of typ_bind list (* service reference *)

and typ_field = typ_field' Source.phrase
and typ_field' = { id : Stdint.uint64; name : id; typ : typ }

and typ_bind = (typ_bind', Type.typ) Source.annotated_phrase
and typ_bind' = {var : id; bound : typ}

(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | TypD of id * typ             (* type *)
  | ActorD of id * typ_bind list     (* service *)

(* Program *)

type prog = prog' Source.phrase
and prog' = dec list


(* n-ary arguments/result sequences *)

let seqT ts =
  match ts with
  | [t] -> t
  | ts ->
    { Source.it = TupT ts;
      at = Source.no_region;
      Source.note = Type.Tup (List.map (fun t -> t.Source.note) ts) }

let as_seqT t =
  match t.Source.it with
  | TupT ts -> ts
  | _ -> [t]

