(* Synchronous compute plugins. The native driver installs trusted compiler
   extensions before loading Prim. Results are ordinary managed Motoko values.
 *)
open Mo_types

(* [Blob] preserves the ordinary first-class Blob -> Blob API. [Shared] and
   [Fixed] are compiler-specialized intrinsics, including through aliases. *)
type arguments = Blob | Shared | Fixed of Type.typ list

type specialization = {
  type_arguments : Type.typ list;
  argument_types : Type.typ list;
  result_type : Type.typ;
}

type binding = {
  id : string;
  name : string;
  arguments : arguments;
  type_parameters : int;
  result_type : Type.typ list -> Type.typ list -> (Type.typ, string) result;
  evaluate : Type.typ list -> Type.typ list -> Value.value list ->
    (Value.value, string) result;
}

val register : binding -> unit
val find : string -> binding option
val declarations : unit -> string
val intrinsic_type : string -> Type.typ
val intrinsic : Type.typ -> string option
val specialize : binding -> Type.typ list -> Type.typ list ->
  (specialization, string) result
val map_specialization : (Type.typ -> Type.typ) -> specialization ->
  specialization
val evaluate : string -> specialization -> Value.value list ->
  (Value.value, string) result
