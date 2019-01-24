(* A common data type for diagnostic messages *)

type severity = Warning | Error

type message = {
  sev : severity;
  at : Source.region;
  cat : string;
  text : string
}

type messages = message list

val fatal_error : Source.region -> string -> message

val string_of_message : message -> string
val print_message : message -> unit
val print_messages : messages -> unit

(*
An extension of the built-in result type that also reports diagnostic messages.
Both success and failure can come with messages)
*)

type 'a result = ('a * messages, messages) Pervasives.result

val map_result : ('a -> 'b) -> 'a result -> 'b result

(*
An impure, but more more convenient interface.

The 'result' type above is a monad, and would be sufficient to model, e.g., the
type checker. But since monadic style is cumbersome, the following definitions
provide an impure way of tracking messages.

The function with_message_store returns Error if its argument returns None or
the reported messages contain an error.
*)

type msg_store (* abstract *)
val add_msg : msg_store -> message -> unit
val with_message_store : (msg_store -> 'a option) -> 'a result

