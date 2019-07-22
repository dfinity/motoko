open As_types

(* Numeric Representations *)

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
  val pow : t -> t -> t
  val to_string : t -> string
  val to_pretty_string : t -> string
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
  val to_big_int : t -> Big_int.big_int
  val of_big_int : Big_int.big_int -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_pretty_string : t -> string
end

module type FloatType =
sig
  include Wasm.Float.S
  val pow : t -> t -> t
  val to_pretty_string : t -> string
end

module Word8 : WordType with type bits = int32
module Word16 : WordType with type bits = int32
module Word32 : WordType with type bits = int32 and type t = Wasm.I32.t
module Word64 : WordType with type bits = int64 and type t = Wasm.I64.t
module Float : FloatType with type bits = int64 and type t = Wasm.F64.t

module Nat : NumType with type t = Big_int.big_int
module Int : NumType with type t = Big_int.big_int
module Int_8 : NumType
module Int_16 : NumType
module Int_32 : NumType
module Int_64 : NumType
module Nat8 : NumType
module Nat16 : NumType
module Nat32 : NumType
module Nat64 : NumType


(* Environment *)

module Env : Env.S with type key = string


(* Types *)

type unicode = int

type func = value -> value cont -> unit
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
  | Func of Call_conv.t * func
  | Async of async
  | Mut of value ref
  | Serialized of value

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Shorthands *)

val unit : value

val local_func : int -> int -> func -> value
val message_func : int -> func -> value
val async_func : int -> func -> value


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
val as_func : value -> Call_conv.t * func
val as_async : value -> async
val as_mut : value -> value ref
val as_serialized : value -> value


(* Ordering *)

val equal : value -> value -> bool
val compare : value -> value -> int


(* Pretty Printing *)

val string_of_val : int -> value -> string
val string_of_def : int -> def -> string

