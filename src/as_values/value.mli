open As_types

(* Numeric Representations *)

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
  val pow : t -> t -> t
  val to_string : t -> string
end

module type NumType =
sig
  type t
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
  val of_string : string -> t
  val to_string : t -> string
end

module type FloatType =
sig
  include Wasm.Float.S
  val pow : t -> t -> t
end

module Word8 : WordType with type bits = int32
module Word16 : WordType with type bits = int32
module Word32 : WordType with type bits = int32 and type t = Wasm.I32.t
module Word64 : WordType with type bits = int64 and type t = Wasm.I64.t
module Float : FloatType with type bits = int64 and type t = Wasm.F64.t

module Nat : NumType with type t = Big_int.big_int
module Int : NumType with type t = Big_int.big_int


(* Environment *)

module Env : Env.S with type key = string


(* Types *)

type unicode = int

type call_conv = {
  sort: Type.sharing;
  control : Type.control;
  n_args : int;
  n_res : int;
}

val call_conv_of_typ : Type.typ -> call_conv

type func = value -> value cont -> unit
and value =
  | Null
  | Bool of bool
  | Int of Int.t
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Tup of value list
  | Opt of value
  | Variant of string * value
  | Array of value array
  | Obj of value Env.t
  | Func of call_conv * func
  | Async of async
  | Mut of value ref
  | Serialized of value

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Shorthands *)

val unit : value


(* Smart constructors *)

val local_cc : int -> int -> call_conv
val message_cc : int -> call_conv
val async_cc : int -> call_conv

val local_func : int -> int -> func -> value
val message_func : int -> func -> value
val async_func : int -> func -> value

(* Projections *)

val as_null : value -> unit
val as_bool : value -> bool
val as_int : value -> Int.t
val as_word8 : value -> Word8.t
val as_word16 : value -> Word16.t
val as_word32 : value -> Word32.t
val as_word64 : value -> Word64.t
val as_float : value -> Float.t
val as_char : value -> unicode
val as_text : value -> string
val as_array : value -> value array
val as_tup : value -> value list
val as_unit : value -> unit
val as_pair : value -> value * value
val as_opt : value -> value
val as_obj : value -> value Env.t
val as_variant : value -> string * value
val as_func : value -> call_conv * func
val as_async : value -> async
val as_mut : value -> value ref
val as_serialized : value -> value


(* Ordering *)

val equal : value -> value -> bool
val compare : value -> value -> int


(* Pretty Printing *)

val string_of_val : int -> value -> string
val string_of_def : int -> def -> string
val string_of_call_conv : call_conv -> string
