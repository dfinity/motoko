(* Numeric Representations *)

module type WordType =
sig
  include Wasm.Int.S
  val neg : t -> t
  val not : t -> t
end

module type NumType =
sig
  include module type of Z
  val sub : t -> t -> t
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
end

module Word8 : WordType with type bits = int32
module Word16 : WordType with type bits = int32
module Word32 : WordType with type bits = int32 and type t = Wasm.I32.t
module Word64 : WordType with type bits = int64 and type t = Wasm.I64.t
module Float : Wasm.Float.S with type bits = int64 and type t = Wasm.F64.t

module Nat : NumType
module Int : NumType


(* Environment *)

module Env : Env.S with type key = string


(* Types *)

type unicode = int

type value =
  | Null
  | Bool of bool
  | Nat of Nat.t
  | Int of Int.t
  | Word8 of Word8.t
  | Word16 of Word16.t
  | Word32 of Word32.t
  | Word64 of Word64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Tup of value list
  | Obj of def Env.t
  | Array of value array
  | Opt of value option (* TBR *)
  | Func of (value -> value cont -> unit)
  | Async of async
  | Mut of value ref

and def = value option ref
and async = {mutable result : value option; mutable waiters : value cont list}
and 'a cont = 'a -> unit


(* Projections *)

val unit : value

val as_null : value -> unit
val as_bool : value -> bool
val as_nat : value -> Nat.t
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
val as_obj : value -> def Env.t
val as_opt : value -> value option
val as_func : value -> (value -> value cont -> unit)
val as_async : value -> async
val as_mut : value -> value ref


(* Pretty Printing *)

val string_of_val : Type.kind Con.Env.t -> Type.typ -> value -> string

val debug_string_of_val : value -> string
val debug_string_of_def : def -> string
val debug_string_of_tuple_val : value -> string
