(* Representation *)

type control = Returns | Promises (* returns a computed value or immediate promise *)
type sharing = Local | Sharable
type obj_sort = Object of sharing | Actor
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
  | Obj of obj_sort * field list              (* object *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of sharing * control *
            bind list * typ list * typ list   (* function *)
  | Async of typ                              (* future *)
  | Mut of typ                                (* mutable type *)
  | Shared                                    (* sharable *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Pre                                       (* pre-type *)

and bind = {var : string; bound : typ}
and field = {name : string; typ : typ}

(* cons and kinds *)

and kind_field (* abstract *)
and con = kind_field Con.t
and kind =
  | Def of bind list * typ
  | Abs of bind list * typ

module ConSet : Set.S with type elt = con
type con_set = ConSet.t

val kind : con -> kind
val fresh_con : string -> kind -> con
val set_kind : con -> kind -> unit
val modify_kind : con -> (kind -> kind) -> unit

(* field ordering *)

val compare_field : field -> field -> int


(* n-ary argument/result sequences *)

val seq: typ list -> typ
val as_seq : typ -> typ list


(* Short-hands *)

val unit : typ
val bool : typ
val nat : typ
val int : typ

val prim : string -> prim


(* Projection *)

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
val as_obj : typ -> obj_sort * field list
val as_array : typ -> typ
val as_opt : typ -> typ
val as_tup : typ -> typ list
val as_unit : typ -> unit
val as_pair : typ -> typ * typ
val as_func : typ -> sharing * control * bind list * typ list * typ list
val as_async : typ -> typ
val as_mut : typ -> typ
val as_immut : typ -> typ

val as_prim_sub : prim -> typ -> unit
val as_obj_sub : string -> typ -> obj_sort * field list
val as_array_sub : typ -> typ
val as_opt_sub : typ -> typ
val as_tup_sub : int -> typ -> typ list
val as_unit_sub : typ -> unit
val as_pair_sub : typ -> typ * typ
val as_func_sub : int -> typ -> bind list * typ * typ
val as_mono_func_sub : typ -> typ * typ
val as_async_sub : typ -> typ

val lookup_field : string -> field list -> typ

val span : typ -> int option


(* Normalization and Classification *)

val normalize : typ -> typ
val promote : typ -> typ

exception Unavoidable of con
val avoid : con_set -> typ -> typ (* raise Unavoidable *)


(* Equivalence and Subtyping *)

val eq : typ -> typ -> bool
val sub : typ -> typ -> bool

val lub : typ -> typ -> typ
val glb : typ -> typ -> typ


(* First-order substitution *)

val close : con list -> typ -> typ
val close_binds : con list -> bind list -> bind list

val open_ : typ list -> typ -> typ
val open_binds : bind list -> typ list


(* Environments *)

module Env : Env.S with type key = string


(* Pretty printing *)

val string_of_prim : prim -> string
val string_of_sharing: sharing -> string
val string_of_typ : typ -> string
val string_of_kind : kind -> string
val strings_of_kind : kind -> string * string * string

val string_of_typ_expand : typ -> string
