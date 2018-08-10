(* Representation *)

type con = Con.t
type mut = Const | Mut
type actor = Object | Actor

type prim =
  | Null
  | Bool
  | Nat
  | Int
  | Word8
  | Word16
  | Word32
  | Word64
  | Float
  | Char
  | Text

type t = typ
and typ =
  | Var of con * typ list                     (* constructor *)
  | Prim of prim                              (* primitive *)
  | Obj of actor * field list                 (* object *)
  | Array of mut * typ                        (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of bind list * typ * typ             (* function *)
  | Async of typ                              (* future *)
  | Like of typ                               (* expansion *)
  | Any                                       (* top *)
(*
  | Union of type * typ                       (* union *)
  | Atom of string                            (* atom *)
*)

and bind = {con : con; bound : typ}
and field = {lab : string; typ : typ; mut : mut}

type kind =
  | Def of bind list * typ
  | Abs of bind list * typ

type con_env = kind Con.Env.t


(* Short-hands *)

val unit : typ
val bool : typ
val nat : typ
val int : typ


(* Equivalence and Normalization *)

val eq : con_env -> typ -> typ -> bool
val normalize : con_env -> typ -> typ
val structural : con_env -> typ -> typ


(* First-order substitution *)

type subst = typ Con.Env.t

val subst : subst -> typ -> typ
val subst_binds : subst -> bind list -> bind list
val make_subst : typ list -> bind list -> subst


(* Environments *)

module Env : Env.S with type key = string


(* Pretty printing *)

val string_of_mut : mut -> string
val string_of_prim : prim -> string
val string_of_typ : typ -> string
val string_of_kind : kind -> string
