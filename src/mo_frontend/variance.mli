open Mo_types

(* Polarities of variables *)

(*
  Given a type, a variable is
  * Neutral if it has no occurrence
  * Pos(itive) if it only occurs in covariant position
  * Neg(ative) if it only occurs in contravariant positions
  * Invariant if it occurs in both positive and negative positions
*)

type t = Neutral | Pos | Neg | Invariant

(* `polarities cons typ` maps each variable in `cons` to
    its polarity in `typ`
*)
val polarities : Type.ConSet.t -> Type.typ -> t Type.ConEnv.t
