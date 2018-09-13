(* Representation *)

type con = Con.t
type sort = Object | Actor
type eff = Triv | Await

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
  | Var of string * int                       (* variable *)
  | Con of con * typ list                     (* constructor *)
  | Prim of prim                              (* primitive *)
  | Obj of sort * field list                  (* object *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of bind list * typ * typ             (* function *)
  | Async of typ                              (* future *)
  | Like of typ                               (* expansion *)
  | Mut of typ                                (* mutable type *)
  | Any                                       (* top *)
  | Pre                                       (* pre-type *)

and bind = {var : string; bound : typ}
and field = {name : string; typ : typ}

type kind =
  | Def of bind list * typ
  | Abs of bind list * typ

type con_env = kind Con.Env.t


(* Short-hands *)

val unit : typ
val bool : typ
val nat : typ
val int : typ

val prim : string -> prim


(* Inspection & Projection *)

val is_prim : prim -> typ -> bool
val is_obj : typ -> bool
val is_array : typ -> bool
val is_opt : typ -> bool
val is_tup : typ -> bool
val is_unit : typ -> bool
val is_pair : typ -> bool
val is_func : typ -> bool
val is_async : typ -> bool
val is_mut : typ -> bool

val as_prim : prim -> typ -> unit
val as_obj : typ -> sort * field list
val as_array : typ -> typ
val as_opt : typ -> typ
val as_tup : typ -> typ list
val as_unit : typ -> unit
val as_pair : typ -> typ * typ
val as_func : typ -> bind list * typ * typ
val as_async : typ -> typ
val as_mut : typ -> typ
val as_immut : typ -> typ

val as_prim_sub : prim -> con_env -> typ -> unit
val as_obj_sub : con_env -> typ -> sort * field list
val as_array_sub : con_env -> typ -> typ
val as_opt_sub : con_env -> typ -> typ
val as_tup_sub : con_env -> typ -> typ list
val as_unit_sub : con_env -> typ -> unit
val as_pair_sub : con_env -> typ -> typ * typ
val as_func_sub : con_env -> typ -> bind list * typ * typ
val as_mono_func_sub : con_env -> typ -> typ * typ
val as_async_sub : con_env -> typ -> typ

val lookup_field : string -> field list -> typ


(* Normalization and Classification *)

val normalize : con_env -> typ -> typ
val structural : con_env -> typ -> typ

exception Unavoidable of con
val avoid : con_env -> con_env -> typ -> typ (* raise Unavoidable *)


(* Equivalence and Subtyping *)

val eq : con_env -> typ -> typ -> bool
val sub : con_env -> typ -> typ -> bool

val join : con_env -> typ -> typ -> typ
val meet : con_env -> typ -> typ -> typ


(* First-order substitution *)

val close : con list -> typ -> typ
val close_binds : con list -> bind list -> bind list

val open_ : typ list -> typ -> typ
val open_binds : con_env -> bind list -> typ list * con_env


(* Environments *)

module Env : Env.S with type key = string


(* Pretty printing *)

val string_of_prim : prim -> string
val string_of_typ : typ -> string
val string_of_kind : kind -> string
val strings_of_kind : kind -> string * string * string

val string_of_typ_expand : con_env -> typ -> string
