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
type class_

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
  | Array of value array
  | Obj of class_ option * value Env.t
  | Func of class_ option * func
  | Async of async
  | Mut of value ref

and async = {result : def; mutable waiters : value cont list}
and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Shorthands *)

val unit : value


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
val as_obj : value -> class_ option * value Env.t
val as_func : value -> class_ option * (value -> value cont -> unit)
val as_async : value -> async
val as_mut : value -> value ref


(* Classes *)

val new_class : unit -> class_


(* Ordering *)

val equal : value -> value -> bool
val compare : value -> value -> int


(* Pretty Printing *)

val string_of_val : value -> string
val string_of_def : def -> string
