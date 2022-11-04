open Mo_types

(* Variance of type variables *)

(*
  Given a type, a variable is
  * Bivariant if it has no occurrence
  * Covariant if it only occurs in positive positions
  * Contraviant if it only occurs in negative positions
  * Invariant if it occurs in both positive and negative positions
*)

type t = Bivariant | Covariant | Contravariant | Invariant

(* `variances cons typ` maps each variable in `cons` to
    its variance in `typ`
*)
val variances : Type.ConSet.t -> Type.typ -> t Type.ConEnv.t
