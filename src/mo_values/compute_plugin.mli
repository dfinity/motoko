(* Synchronous compute plugins. The native driver installs trusted compiler
   extensions before loading Prim. Results are ordinary managed Blobs. *)
open Mo_types

type arguments = Blob | Shared

type binding = {
  id : string;
  name : string;
  arguments : arguments;
  evaluate : Type.typ list -> Value.value list -> (string, string) result;
}

val register : binding -> unit
val find : string -> binding option
val declarations : unit -> string
val intrinsic_type : string -> Type.typ
val intrinsic : Type.typ -> string option
val check : binding -> Type.typ list -> (unit, string) result
val evaluate : string -> Type.typ list -> Value.value list ->
  (string, string) result
