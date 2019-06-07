(* Calling convention *)

type call_conv = {
  sort: Type.sharing;
  control : Type.control;
  n_args : int;
  n_res : int;
}
type t = call_conv

val local_cc : int -> int -> call_conv
val message_cc : int -> call_conv
val async_cc : int -> call_conv

val call_conv_of_typ : Type.typ -> call_conv

val string_of_call_conv : call_conv -> string
