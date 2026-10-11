open Mo_values

(** Direct canonical Candid encoding for the fixed cx profile. *)

(** Encode the interpreter's original typed values directly. Value-derived
    finite shapes are emitted without ordinary serialization or wire parsing. *)
val encode_values : Mo_types.Type.typ list -> Value.value list ->
  (bytes, string) result
