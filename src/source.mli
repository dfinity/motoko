type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}
type ('a, 'b) annotated_phrase = {at : region; it : 'a; mutable note: 'b}
type 'a phrase = ('a, unit) annotated_phrase

val no_pos : pos
val no_region : region

val string_of_pos : pos -> string
val string_of_region : region -> string

val span : region -> region -> region

val (@@) : 'a -> region -> 'a phrase

module Counters : sig
  type t
  val zeros : unit -> t
  val bump : t -> region -> unit
  val dump : t -> unit
end
