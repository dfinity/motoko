(* Representation *)

type lab = string
type var = string

type control = Returns | Promises | Replies
type obj_sort = Object | Actor | Module | Memory
type shared_sort = Query | Write
type 'a shared = Local | Shared of 'a
type func_sort = shared_sort shared
type eff = Triv | Await

type prim =
  | Null
  | Bool
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
  | Float
  | Char
  | Text
  | Blob (* IR use: Packed representation, vec u8 IDL type *)
  | Error
  | Principal

type t = typ

and typ =
  | Var of var * int                          (* variable *)
  | Con of con * typ list                     (* constructor *)
  | Prim of prim                              (* primitive *)
  | Obj of obj_sort * field list              (* object *)
  | Variant of field list                     (* variant *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of func_sort * control * bind list * typ list * typ list  (* function *)
  | Async of scope * typ                        (* future *)
  | Mut of typ                                (* mutable type *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Typ of con                                (* type (field of module) *)
  | Pre                                       (* pre-type *)

and scope = typ

and bind_sort = Scope | Type
and bind = {var : var; sort: bind_sort; bound : typ}

and field = {lab : lab; typ : typ; depr : string option}

and con = kind Cons.t
and kind =
  | Def of bind list * typ
  | Abs of bind list * typ

(* Function sorts *)

val is_shared_sort : 'a shared -> bool

(* Short-hands *)

val unit : typ
val bool : typ
val nat : typ
val nat64 : typ
val int : typ
val text : typ
val blob : typ
val error : typ
val char : typ
val principal : typ

val throwErrorCodes : field list
val catchErrorCodes : field list
val throw : typ
val catch : typ

val caller : typ
val ctxt: typ

val iter_obj : typ -> typ

val prim : string -> prim


(* Projection *)

val is_non : typ -> bool
val is_prim : prim -> typ -> bool
val is_obj : typ -> bool
val is_module : typ -> bool
val is_immutable_obj : typ -> bool
val is_variant : typ -> bool
val is_array : typ -> bool
val is_opt : typ -> bool
val is_tup : typ -> bool
val is_unit : typ -> bool
val is_pair : typ -> bool
val is_func : typ -> bool
val is_async : typ -> bool
val is_mut : typ -> bool
val is_typ : typ -> bool
val is_con : typ -> bool

val as_prim : prim -> typ -> unit
val as_obj : typ -> obj_sort * field list
val as_variant : typ -> field list
val as_array : typ -> typ
val as_opt : typ -> typ
val as_tup : typ -> typ list
val as_unit : typ -> unit
val as_pair : typ -> typ * typ
val as_func : typ -> func_sort * control * bind list * typ list * typ list
val as_async : typ -> typ * typ
val as_mut : typ -> typ
val as_immut : typ -> typ
val as_typ : typ -> con
val as_con : typ -> con * typ list

val as_prim_sub : prim -> typ -> unit
val as_obj_sub : string list -> typ -> obj_sort * field list
val as_variant_sub : string -> typ -> field list
val as_array_sub : typ -> typ
val as_opt_sub : typ -> typ
val as_tup_sub : int -> typ -> typ list
val as_unit_sub : typ -> unit
val as_pair_sub : typ -> typ * typ
val as_func_sub : func_sort -> int -> typ -> func_sort * bind list * typ * typ
val as_mono_func_sub : typ -> typ * typ
val as_async_sub : typ -> typ -> typ * typ


(* Argument/result sequences *)

val seq : typ list -> typ
val codom : control -> (unit -> typ) -> typ list -> typ
val as_seq : typ -> typ list (* This needs to go away *)
val seq_of_tup : typ -> typ list
val arity : typ -> int


(* Fields *)

val lookup_val_field : string -> field list -> typ
val lookup_typ_field : string -> field list -> con
val lookup_val_field_opt : string -> field list -> typ option
val lookup_typ_field_opt : string -> field list -> con option

val lookup_val_deprecation : string -> field list -> string option
val lookup_typ_deprecation : string -> field list -> string option

val compare_field : field -> field -> int


(* Constructors *)

val set_kind : con -> kind -> unit

module ConEnv : Env.S with type key = con
module ConSet : Dom.S with type elt = con


(* Sets *)

module S : Set.S with type elt = typ


(* Normalization and Classification *)

val normalize : typ -> typ
val promote : typ -> typ

val opaque : typ -> bool
val concrete : typ -> bool
val shared : typ -> bool
val find_unshared : typ -> typ option
val is_shared_func : typ -> bool

val stable : typ -> bool

val inhabited : typ -> bool
val singleton : typ -> bool
val span : typ -> int option


(* Constructor occurrences *)

val cons: typ -> ConSet.t
val cons_kind : kind -> ConSet.t


(* Equivalence and Subtyping *)

val eq : typ -> typ -> bool
val eq_kind : kind -> kind -> bool

val sub : typ -> typ -> bool
val compatible : typ -> typ -> bool

exception PreEncountered
val lub : typ -> typ -> typ
val glb : typ -> typ -> typ


(* First-order substitution *)

val close : con list -> typ -> typ
val close_binds : con list -> bind list -> bind list

val open_ : typ list -> typ -> typ
val open_binds : bind list -> typ list


(* Environments *)

module Env : Env.S with type key = string


(* Scope bindings *)

val scope_var : var -> var
val default_scope_var : var
val scope_bound : typ
val scope_bind : bind

(* Signatures *)

val match_stab_sig : field list -> field list -> bool

val string_of_stab_sig : field list -> string

(* Well-known fields *)

val motoko_async_helper_fld : field
val motoko_stable_var_size_fld : field
val get_candid_interface_fld : field

val well_known_actor_fields : field list
val decode_msg_typ : field list -> typ

(* Pretty printing *)

val string_of_prim : prim -> string
val string_of_obj_sort : obj_sort -> string
val string_of_func_sort : func_sort -> string

module type Pretty = sig
  val pp_typ : Format.formatter -> typ -> unit
  val pp_typ_expand : Format.formatter -> typ -> unit
  val pps_of_kind : kind ->
    string *
    (Format.formatter -> unit -> unit) *
    (Format.formatter -> unit -> unit)
  val string_of_con : con -> string
  val string_of_typ : typ -> string
  val string_of_kind : kind -> string
  val strings_of_kind : kind -> string * string * string
  val string_of_typ_expand : typ -> string
end

module type PrettyConfig = sig
  val show_stamps : bool
  val con_sep : string
  val par_sep : string
end

module ShowStamps : PrettyConfig

module ElideStamps : PrettyConfig

module ParseableStamps : PrettyConfig

module MakePretty(_ : PrettyConfig) : Pretty

include Pretty
