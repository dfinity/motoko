open Mo_types

(* Numeric Representations *)

module type NumType =
sig
  type t
  val signed : bool
  val zero : t
  val abs : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val pow : t -> t -> t
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val compare : t -> t -> int
  val to_int : t -> int
  val of_int : int -> t
  val to_big_int : t -> Big_int.big_int
  val of_big_int : Big_int.big_int -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_pretty_string : t -> string
end

(* Extension of NumType with wrapping and bit-wise operations *)
module type BitNumType =
sig
  include NumType

  val not : t -> t
  val popcnt : t -> t
  val clz : t -> t
  val ctz : t -> t

  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val shl : t -> t -> t
  val shr : t -> t -> t
  val rotl : t -> t -> t
  val rotr : t -> t -> t

  val wrapping_neg : t -> t
  val wrapping_add : t -> t -> t
  val wrapping_sub : t -> t -> t
  val wrapping_mul : t -> t -> t
  val wrapping_div : t -> t -> t
  val wrapping_pow : t -> t -> t
end

module type FloatType =
sig
  include Wasm.Float.S
  val rem : t -> t -> t
  val pow : t -> t -> t
  val to_pretty_string : t -> string
end

module Float : FloatType with type bits = int64 and type t = Wasm.F64.t

module Nat : NumType with type t = Big_int.big_int
module Int : NumType with type t = Big_int.big_int
module Int_8 : BitNumType
module Int_16 : BitNumType
module Int_32 : BitNumType
module Int_64 : BitNumType
module Nat8 : BitNumType
module Nat16 : BitNumType
module Nat32 : BitNumType
module Nat64 : BitNumType

module Blob : sig
  val escape : string -> string
end

(* Environment *)

module Env : Env.S with type key = string


(* Types *)

type unicode = int
type actor_id = string

type context = value

and func =
   context -> value -> value cont -> unit

and value =
  | Null
  | Bool of bool
  | Int of Int.t
  | Int8 of Int_8.t
  | Int16 of Int_16.t
  | Int32 of Int_32.t
  | Int64 of Int_64.t
  | Nat8 of Nat8.t
  | Nat16 of Nat16.t
  | Nat32 of Nat32.t
  | Nat64 of Nat64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Blob of string
  | Tup of value list
  | Opt of value
  | Variant of string * value
  | Array of value array
  | Obj of value Env.t
  | Func of Call_conv.t * func
  | Async of async
  | Mut of value ref
  | Iter of value Seq.t ref (* internal to {b.bytes(), t.chars()} iterator *)

and res = Ok of value | Error of value
and async = {result : res Lib.Promise.t ; mutable waiters : (value cont * value cont) list}

and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Shorthands *)

val unit : value

val local_func : int -> int -> func -> value
val message_func : Type.shared_sort -> int -> func -> value
val async_func : Type.shared_sort -> int -> int -> func -> value
val replies_func : Type.shared_sort -> int -> int -> func -> value


(* Pseudo actor ids *)

val fresh_id : unit -> actor_id
val top_id : actor_id


(* Projections *)

val as_null : value -> unit
val as_bool : value -> bool
val as_int : value -> Int.t
val as_int8 : value -> Int_8.t
val as_int16 : value -> Int_16.t
val as_int32 : value -> Int_32.t
val as_int64 : value -> Int_64.t
val as_nat8 : value -> Nat8.t
val as_nat16 : value -> Nat16.t
val as_nat32 : value -> Nat32.t
val as_nat64 : value -> Nat64.t
val as_float : value -> Float.t
val as_char : value -> unicode
val as_text : value -> string
val as_blob : value -> string
val as_iter : value -> value Seq.t ref
val as_array : value -> value array
val as_tup : value -> value list
val as_unit : value -> unit
val as_pair : value -> value * value
val as_opt : value -> value
val as_obj : value -> value Env.t
val as_variant : value -> string * value
val as_func : value -> Call_conv.t * func
val as_async : value -> async
val as_mut : value -> value ref


(* Ordering *)

val equal : value -> value -> bool
val compare : value -> value -> int


(* Pretty Printing *)

val string_of_val : int -> value -> string
val string_of_def : int -> def -> string
