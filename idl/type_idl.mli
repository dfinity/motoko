(* Representation *)

type lab = string
type var = string

type mode = Sensitive | Pure | Updatable

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

type t = typ
and typ =
  | Var of var                          (* variable *)
  | Prim of prim                              (* primitive *)
  | Vec of typ                              (* vector *)
  | Opt of typ                                (* option *)
  | Variant of field list               (* variant *)
  | Record of field list (* record *)
  | Tup of typ list                           (* tuple *)
  | Func of mode list * typ list * typ list  (* function *)
  | RefFunc of typ (* function reference *)
  | RefServ of bind list (* service reference *)
  | Serialized of typ                         (* a serialized value *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Pre                                       (* pre-type *)

and bind = {var : var; bound : typ}
and field = {lab : lab; typ : typ}

(* Environments *)

module Env : Env.S with type key = string

(* Short-hands *)

val unit : typ
val bool : typ
val nat : typ
val int : typ

val prim : string -> prim

(*
(* Projection *)

val is_prim : prim -> typ -> bool
val is_obj : typ -> bool
val is_variant : typ -> bool
val is_array : typ -> bool
val is_opt : typ -> bool
val is_tup : typ -> bool
val is_unit : typ -> bool
val is_pair : typ -> bool
val is_func : typ -> bool
val is_async : typ -> bool
val is_mut : typ -> bool
val is_serialized : typ -> bool

val as_prim : prim -> typ -> unit
val as_obj : typ -> obj_sort * field list
val as_variant : typ -> (lab * typ) list
val as_array : typ -> typ
val as_opt : typ -> typ
val as_tup : typ -> typ list
val as_unit : typ -> unit
val as_pair : typ -> typ * typ
val as_func : typ -> sharing * control * bind list * typ list * typ list
val as_async : typ -> typ
val as_mut : typ -> typ
val as_immut : typ -> typ
val as_serialized : typ -> typ

val as_prim_sub : prim -> typ -> unit
val as_obj_sub : string -> typ -> obj_sort * field list
val as_array_sub : typ -> typ
val as_opt_sub : typ -> typ
val as_tup_sub : int -> typ -> typ list
val as_unit_sub : typ -> unit
val as_pair_sub : typ -> typ * typ
val as_func_sub : sharing -> int -> typ -> sharing * bind list * typ * typ
val as_mono_func_sub : typ -> typ * typ
val as_async_sub : typ -> typ

val seq : typ list -> typ
val as_seq : typ -> typ list
val inst_func_type : typ -> sharing -> typ list -> (typ * typ)

val lookup_field : string -> field list -> typ
val compare_field : field -> field -> int

val map_constr_typ : (typ -> typ) -> (lab * typ) list -> (lab * typ) list
val compare_summand : (lab * typ) -> (lab * typ) -> int

val span : typ -> int option


(* Constructors *)

val set_kind : con -> kind -> unit

module ConSet : Dom.S with type elt = con


(* Normalization and Classification *)

val normalize : typ -> typ
val promote : typ -> typ

exception Unavoidable of con
val avoid : ConSet.t -> typ -> typ (* raise Unavoidable *)

val is_concrete : typ -> bool

(* Equivalence and Subtyping *)

val eq : typ -> typ -> bool
val eq_kind : kind -> kind -> bool

val sub : typ -> typ -> bool

val lub : typ -> typ -> typ
val glb : typ -> typ -> typ


(* First-order substitution *)

val close : con list -> typ -> typ
val close_binds : con list -> bind list -> bind list

val open_ : typ list -> typ -> typ
val open_binds : bind list -> typ list


(* Pretty printing *)

val string_of_prim : prim -> string
val string_of_sharing: sharing -> string
val string_of_typ : typ -> string
val string_of_kind : kind -> string
val strings_of_kind : kind -> string * string * string

val string_of_typ_expand : typ -> string
 *)
