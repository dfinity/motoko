(* (Stable) Signature matching *)

open Mo_types

(* signature matching with multiple error reporting
   c.f. (simpler) Types.match_stab_sig.
*)

val match_stab_sig : Type.field list -> Type.field list -> unit Diag.result
